% WOOPER: Wrapper for OOP in ERlang.

% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date: Friday, July 6, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL, see:
% http://ceylan.sourceforge.net/main/documentation/wooper/index.html#license


% Provides most classical constructs: new/delete operators, remote method 
% invocation (RMI), polymorphism and multiple inheritance, all with state
% management and in a quite efficient way (i.e. no significantly faster 
% approach in Erlang could be imagined by the author).

% Instances are created thanks to the new operator, which calls automatically
% the relevant constructor ('construct' function).
% Abstract classes need only to define their 'construct' function, as no 'new'
% operator can apply.

% A class C is mapped to an Erlang module, preferably named 'class_C'.
% An active object is mapped to an Erlang process.
% Methods support Remote Invocation Calls, mapped to Erlang messages.
% Inheritance is implemented thanks to a per-class method virtual table,
% including the locally-defined ones and all the inherited ones.
% This table is shared among all the instances of a given class, thanks to
% a singleton-like class manager process that keeps references to the virtual
% table of each class. 
% Instance state is maintained thanks to a per-instance attribute table,
% storing all its attributes, including all the inherited ones.
%
% The hashtable type, defined in hashtable.erl, is used at all levels: 
% per-instance (for the attribute table), per-class (for the so-called virtual
% table), per-node (for the class manager).

% When an exported function is called as a method (i.e. it is listed in 
% the wooper_method_export variable, see below) the list of parameters
% being received is prefixed with the instance state (a bit like 'self' in
% Python): A ! { aMethod, [1,2] } results in the calling of the 'aMethod'
% function defined in the class module of A (exported thanks to
% wooper_method_export) with parameters automatically given to that function
% being: 'CurrentStateOfA, 1, 2' instead of '1, 2', with 
% CurrentStateOfA being the A state variable automatically kept in the instance
% main loop.
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2
% (it is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is
% called with only two parameters specified (state not being included).


% The usual content of the '-export([XXX]).' clause in a class module should be
% dispatched in:
%
%   '-define(wooper_method_export,YYY).', to declare methods, ex: 
% '-define(wooper_method_export,getAge/1,setAge/2,declareBirthday/1).'
% Zero arity is not possible since there is at least the 'State' first 
% parameter. So one just increments the number of intended real 
% function-specific parameters in this export. 
% Ex: a function 'setAge' taking in input only one logical parameter, NewAge,
% should actually be defined as 'setAge(State,NewAge) -> [..]' and therefore
% declared as: '-define(wooper_method_export,a/1,setAge/2,b/2).'
% Note: one should not forget, when overloading a method F/A, to specify it in
% wooper_method_export, otherwise its closest ancestor method will be called
% instead. In this case a warning is issued at compilation of the child class:
% 'Warning: function F/A is unused.'
%
%   '-define(wooper_construct_export,new/p,construct/p+1,toString/1).'
% Ex: '-define(wooper_construct_export,new/2,construct/3,toString/1).' to 
% declare the appropriate construction-related functions (new and construct),
% p being the number of parameters defined in the wooper_construct_parameters
% variable. Only the relevant 'construct' function has to be actually 
% defined by the developer: new is automatically defined appropriately
% (see in this file). toString is optional but proved to be often convenient
% for debugging method implementations
%
%	'-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
% functions, that are not methods
    

% Shared code. 
%
% All WOOPER classes should mention their superclasses and their WOOPER 
% exports before the WOOPER header is included.

% Example:
% -module(class_Cat).
% -define(wooper_superclasses,[class_Mammal,class_ViviparousBeing]).
% -define(wooper_method_export,hasWhiskers/1,canEat/2).
% -define(wooper_construct_parameters,Age,Gender,FurColor).
% -define(wooper_construct_export,new/3,construct/4).
% -include("wooper.hrl").
% [...]
% See also: class_Template.erl



% Allows to define WOOPER base variables and methods for that class:



% Implementation notes.


% Records the state of an instance.
% Module is the Erlang module the class is mapped to.
% This is the class-specific object state, each instance of this class
% will have its own state_holder, quite similar to the 'C++' this pointer.
% Constant data (ex: the virtual table) are referenced by each class instance,
% they are not duplicated (pointer to a virtual table shared by all class
% instances rather than deep copy).
%
% The virtual table holds the method name to module mapping for a given class.
% The attribute table (a hashtable) records all the data members of a given
% instance, including all the inherited ones. 
% The request sender member is used internally by WOOPER so that a request
% method have a way of retrieving the corresponding client PID. This avoids
% the client to specify its PID twice, one for WOOPER, one for the method, as
% a method parameter, in the case the method itself needs the client PID, for
% example to register it in a list in its own state. Thus a client does not have
% to specify: 'MyServer ! {my_request,[self()],self()}', specifying
% 'MyServer ! {my_request,[],self()}' is enough: the method will be able to
% retrieve the client PID thanks to the request_sender member, automatically
% set by WOOPER. For non-request methods (oneways), WOOPER will set
% request_sender to the atom 'undefined', to ensure the oneway crashes whenever
% trying to use this request-specific information to send a message.
-record( state_holder, {
		virtual_table,
		attribute_table,
		request_sender
	}).



% A list could be managed that would allow to discriminate the methods from
% the other exported functions. As macros cannot be substitued in strings
% it would probably force the developer to list them twice.

% The class name, as mapped to a module.
-define(className,?MODULE).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of
% actual attributes for a given class)
-define(WooperAttributeCountUpperBound,32).


% For the name of the registered process that keeps the per-class method 
% hashtables:
-include("wooper_class_manager.hrl").


% WOOPER internal functions.

% Comment/uncomment to respectively disable and enable debug mode:
%-define(wooper_debug,).


% On debug mode, methods will have to return an atom to ensure they
% respect the right format:
-ifdef(wooper_debug).

	% These methods are defined for all classes:
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,wooper_construct_and_run/1,
		is_wooper_debug/0,wooper_debug_listen/3, 
		wooper_display_state/1,wooper_display_virtual_table/1,
		wooper_display_instance/1,
		wooper_get_state_description/1,wooper_get_virtual_table_description/1,
		wooper_get_instance_description/1).
	
	-export([?WooperBaseMethods]).
	
	-ifdef(wooper_method_export).
		-export([?wooper_method_export]).
	-endif.
	
	% Must be defined, but an error message at their call should be clearer:
	-ifdef(wooper_construct_export).
		-export([?wooper_construct_export]).
	-endif.
	

	is_wooper_debug() ->
		true.
	
	% In debug mode, method results are checked thanks to an additional atom:
	-define(wooper_return_state_result(State,Result),
		{wooper_result,State,Result}).
		
	-define(wooper_return_state_only(State),
		{wooper_result,State}).


	wooper_display_loop_state(State) ->
		wooper_display_state(State).

	
-else.

	% Not in debug mode here:

	
	% These methods are defined for all classes:
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,wooper_construct_and_run/1,
		is_wooper_debug/0,wooper_debug_listen/3,wooper_display_state/1,
		wooper_display_virtual_table/1,wooper_display_instance/1).

	-export([?WooperBaseMethods]).

	-ifdef(wooper_method_export).
		-export([?wooper_method_export]).
	-endif.
	
	% Must be defined, but an error message at their call should be clearer:
	-ifdef(wooper_construct_export).
		-export([?wooper_construct_export]).
	-endif.
	

	is_wooper_debug() ->
		false.

	% In release mode, method results are sent directly (no atom added):
	-define(wooper_return_state_result(State,Result),{State,Result}).
	-define(wooper_return_state_only(State),        State).
	
	wooper_display_loop_state(_) ->
		debug_no_activated.
		
-endif.


% Helper function to test requests.
% Allows to test from the shell a server by sending it requests
% (hence needing a receive whereas in the shell).
% Available even when debug mode is off. 
wooper_debug_listen(Pid,Action,Arguments) ->
	Pid ! {Action,Arguments,self()},
	receive

		% A list is assumed to be a string here:
		{wooper_result,Result} when is_list(Result) ->
			io:format("String result of call to '~w' "
				"with arguments '~w': ~s~n", [Action,Arguments,Result]);
		
		{wooper_result,Result} ->
			io:format("Result of call to '~w' with arguments '~w': ~s~n",
				[Action,Arguments,utils:term_toString(Result)]);
	
		Anything ->
			io:format("Answer to call to '~w' with arguments '~w': ~s~n",
				[Action,Arguments,utils:term_toString(Anything)])
	
	end.


% "Static method" (only a function) which returns the name of the class.
get_class_name() ->
	?className.

% "Static method" (only a function) which returns the list of the 
% superclasses for that class.
get_superclasses() ->
	?wooper_superclasses.


% Method that returns the classname of the instance.
get_class_name(State) ->
	?wooper_return_state_result(State,?className).

% Method that returns the superclasses of the instance.
get_superclasses(State) ->
	?wooper_return_state_result(State,?wooper_superclasses).



% Spawns a new instance for this class, using specified parameters to
% construct it.
new(?wooper_construct_parameters) ->
	% Double-list: list with a list in it.
	spawn(?MODULE,wooper_construct_and_run, [[?wooper_construct_parameters]] ).
	

% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run(ParameterList) ->
	% ?MODULE must be specified, otherwise apply/2 returns: {badfun,construct}
	% despite construct is exported with the right arity (do not know why...)
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	wooper_main_loop(
		apply(?MODULE,construct,[BlankTable|ParameterList]) ).
	


% The two following functions have been commented out as their macro 
% counterparts should be faster and do not lead to warnings when they are 
% not used by the class.


% Sets specified attribute of the instance to the specified value, thanks to
% specified state.
% Returns an updated state.
% See also: the similarly named macro.
%setAttribute(State,AttributeName,AttributeValue) ->
%	#state_holder{
%		virtual_table   = State#state_holder.virtual_table,
%		attribute_table = hashtable:addEntry(
%			AttributeName,
%			AttributeValue,
%			State#state_holder.attribute_table ),
%		request_sender  = State#state_holder.request_sender
%	}.	
%

% Sets specified attribute of the instance to the specified value, thanks to
% specified state.
% Returns an updated state.
% See also: the similarly named function.
-define(setAttribute(State,AttributeName,AttributeValue),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntry(
			AttributeName,
			AttributeValue,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName, ClassName }'.
% See also: the similarly named macro.
%getAttribute(State,AttributeName) ->
%	case hashtable:lookupEntry( AttributeName,
%			State#state_holder.attribute_table ) of
%		
%		undefined ->
%			{ attribute_not_found, AttributeName, get_class_name() } ;
%			
%		{value,Value} ->
%			Value
%			
%	end.			


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName, ClassName }'.
% See also: the similarly named function and getAttr/1
-define(getAttribute(State,AttributeName),
	case hashtable:lookupEntry( AttributeName,
			State#state_holder.attribute_table ) of
		
		undefined ->
			{ attribute_not_found, AttributeName, get_class_name() } ;
			
		{value,Value} ->
			Value
			
	end			
).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName, ClassName }'.
% Beware to the implicit use of the 'State' variable: in some cases other
% states should be used. See the getAttribute/2 macro.
-define(getAttr(AttributeName),?getAttribute(State,AttributeName)).


% Returns the Wooper Class Manager.
% If if it is already running, find it and returns its atom, otherwise launch
% it and returns that same atom as well.
wooper_get_class_manager() ->
	case lists:member( ?WooperClassManagerName, registered() ) of
		true ->
			?WooperClassManagerName;
		
		_ ->
			spawn(?WooperClassManagerName,start,[self()]),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			receive
			
				class_manager_registered ->
					?WooperClassManagerName
			
			% 10-second time-out:
			after 10000	->
				io:format( "#### Error: wooper_get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand." ),
				undefined
					
			end
			
	end.		


% Most functions below could be encapsulated in a WOOPER-dedicated module
% (in a .erl instead of a .hrl), but in this case calls may be less efficient.



% Methods for getting informations about an instance.


% Returns a textual representation of the attributes of the specified state.
wooper_state_toString(State) ->
	Attributes = hashtable:enumerate(State#state_holder.attribute_table),
	lists:foldl(
		fun({AttName,AttrValue},Acc) ->
			Acc ++ io_lib:format( 
				"     * ~s = ~s~n", 
				[
					utils:term_toString(AttName),
					utils:term_toString(AttrValue)
				])
			
		end, 
		io_lib:format( "State of ~w:~nInstance of ~s with ~B attribute(s):~n",
			[self(),get_class_name(),length(Attributes)]),
		Attributes).	


% Returns a textual representation of the virtual table corresponding to
% specified state.
wooper_virtual_table_toString(State) ->
	lists:foldl(
		fun( {{Name,Arity},Module}, String ) ->
			String ++ io_lib:format( "     * ~s/~B -> ~s~n",
				[Name,Arity,Module] )
		end,
		io_lib:format( "Virtual table of ~w:~n"
			"(method name/arity -> module defining that method)~n", [self()] ),
		hashtable:enumerate( State#state_holder.virtual_table )). 


% Returns a textual representation of this instance, including its state and
% virtual table.
wooper_instance_toString(State) ->
	io_lib:format( "Inpection of instance ~w:~n~n  + ~s~n  + ~s",
		[ self(), wooper_state_toString(State),
			wooper_virtual_table_toString(State) ]).



% Displays the inner state of this instance.
% This is not a method.
wooper_display_state(State) ->
	io:format( "~s~n", [wooper_state_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_virtual_table(State) ->
	io:format( "~s~n", [wooper_virtual_table_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_instance(State) ->
	io:format( "~s~n", [wooper_instance_toString(State)] ).


-ifdef(wooper_debug).


% Returns a textual description of the inner state of this instance.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_state_description(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).


% Returns a textual description of the virtual table used by this instance.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_virtual_table_description(State) ->
	?wooper_return_state_result( State,	wooper_virtual_table_toString(State) ).
		
		
% Returns a full textual description of this instance, including its state and
% virtual table.
% This is a method for debug-purpose, only activated if wooper_debug is defined.
wooper_get_instance_description(State) ->
	?wooper_return_state_result( State, wooper_instance_toString(State) ).
	
-endif.
	

% Waits for incoming requests and serves them.
wooper_main_loop(State) ->
	wooper_display_loop_state(State),
		
	receive
			
		% Request with response:
		% Server PID could be sent back as well to discriminate 
		% received answers on the client side.
		{ MethodAtom, ArgumentList, SenderPID } 
				when is_pid(SenderPID) and is_list(ArgumentList) ->
			SenderAwareState = State#state_holder{request_sender=SenderPID},
			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, ArgumentList ), 
			%SenderPID ! { self(), Result }
			SenderPID ! Result,
			SenderAgnosticState =
				NewState#state_holder{request_sender=undefined},
			wooper_main_loop(SenderAgnosticState);
		
		% Auto-wrapping single arguments implies putting lists between
		% double-brackets		
		{ MethodAtom, Argument, SenderPID } when is_pid(SenderPID) ->
			SenderAwareState = State#state_holder{request_sender=SenderPID},
			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, [ Argument ] ), 
			%SenderPID ! { self(), Result }
			SenderPID ! Result,
			SenderAgnosticState =
				NewState#state_holder{request_sender=undefined},
			wooper_main_loop(SenderAgnosticState);


		% Oneway calls (no client PID sent, no answer sent back):
		% (either this method does not return anything, or the sender is not
		% interested in the result)
		{ MethodAtom, ArgumentList } when is_list(ArgumentList) ->
			% Any result would be ignored, only the update state is kept:
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, ArgumentList ),
			wooper_main_loop(NewState);
			
		{ MethodAtom, Argument } ->
			% Any result would be ignored, only the update state is kept:
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, [ Argument ] ), 
			wooper_main_loop(NewState);
			
		delete ->
			case hashtable:lookupEntry({delete,1},
					State#state_holder.virtual_table) of 
				
				undefined ->
					% Destructor not overriden, using default one:
					%io:format( "Deleting ~w (default destructor).~n", 
					%	[ self() ]),
					deleted;
				
				{ value, LocatedModule } -> 
					apply( LocatedModule, delete, [ State ] ),
					deleted
					
				
			% (do nothing, loop ended).		
			end;
			
		MethodAtom when is_atom(MethodAtom) ->
			{ NewState, _ } = wooper_execute_method( MethodAtom, State, [] ),
			wooper_main_loop(NewState)

	end.
	% Commented out to preserve (presumably) tail-recursion:
	% io:format( "wooper_main_loop exited.~n" ).

	
	
% Returns the virtual table corresponding to this class.
wooper_retrieve_virtual_table() ->
	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).
	wooper_get_class_manager() ! { get_table, ?MODULE, self() },
	receive
	
		{ virtual_table, ?MODULE, Table } ->
			%hashtable:display(Table),
			Table
	
	end.
	
	
	
	
% Looks-up specified method (Method/Arity, ex: toString/0) to be found 
% in heritance tree and returns either { methodFound, Module } with 
% Module corresponding to the
% class that implements that method, or an error.
% Note: uses the pre-built virtual table for this class.
wooper_lookupMethod(State,MethodAtom,Arity) ->
	hashtable:lookupEntry( {MethodAtom,Arity},
		State#state_holder.virtual_table).



% Following code is duplicated because no '-ifdef' clause can be defined in
% case clauses:


-ifdef(wooper_debug).	
 
 
% Executes the specified method, designated by its atom, with specified 
% instance state and parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom
% 'wooper_method_not_found' is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'wooper_method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {result,Result} is returned, with R being 
% the actual result of the method call, with an updated state.
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returns, which allows a client that sent his
% PID to be warned it is useless, as no answer should be expected.
wooper_execute_method(MethodAtom,State,Parameters) ->	
	%io:format("wooper_execute_method: executing ~s:~s(~w).~n",
	%	[ ?MODULE, MethodAtom, Parameters ]), 	
	% +1: take into account the State additional parameter:
	case wooper_lookupMethod( State, MethodAtom, length(Parameters)+1 ) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:
			case catch apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Matched expressions have to be reordered depending on the
				% debug mode: 


				% Void method (no result returned, only a state):
				% ?wooper_return_state_only:
				{wooper_result,NewState} ->  
					{NewState,wooper_method_returns_void};
				
				% Method returning a result (and a state of course):
				% ?wooper_return_state_result:
				{wooper_result,NewState,Result} ->  			
					{NewState,{wooper_result,Result}};
				
				
				{'EXIT',ErrorTerm} ->
					{State,
						{wooper_method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, ErrorTerm} } ;

				{'EXIT',Pid,ErrorTerm} ->
					{State,
						{wooper_method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, [Pid,ErrorTerm]} } ;

				Other ->
					{State, 
						{wooper_method_faulty_return, self(), ?MODULE,
							MethodAtom,	length(Parameters)+1, 
							Parameters, Other} }	
						
			end;
			
		undefined ->
			% Method name and arity returned as separate tuple elements, as
			% if in a single string ("M/A"), the result is displayed as a list:
			{State, {wooper_method_not_found, ?MODULE, MethodAtom,
				length(Parameters)+1 } }
				
					
		% No other term can be returned.
		
	end.		



-else.

% Not in debug mode here:


% Executes the specified method, designated by its atom, with specified 
% instance state and parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom
% 'wooper_method_not_found' is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'wooper_method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {wooper_result,Result} is returned (with 
% Result being the actual result of the method call) with an updated state.
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returns, which allows a client that sent his
% PID to be warned it is useless, as no answer should be expected.
wooper_execute_method(MethodAtom,State,Parameters) ->
	% +1: take into account the State additional parameter:
	case wooper_lookupMethod(State,MethodAtom,length(Parameters)+1) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:
			case catch apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Matched expressions have to be reordered depending on the
				% debug mode: 
				
				{'EXIT',ErrorTerm} ->
					{State,
						{wooper_method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, ErrorTerm} } ;

				{'EXIT',Pid,ErrorTerm} ->
					{State,
						{wooper_method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, [Pid,ErrorTerm]} } ;


				{NewState,Result} ->  
					{NewState,{wooper_result,Result}};
							
							
				% Void method (no result returned, only a state):
				NewState ->  
					{NewState,wooper_method_returns_void}
						
			end;
			
		undefined ->
			% Method name and arity returned as separate tuple elements, as
			% if in a single string ("M/A"), the result is displayed as a list:
			{State, {wooper_method_not_found, ?MODULE, MethodAtom,
				length(Parameters)+1 } }
				
					
		% No other term can be returned.
		
	end.	
	
		
-endif.
