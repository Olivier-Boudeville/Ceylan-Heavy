% WOOPER : Wrapper for OOP in ERlang.

% See documentation at :
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date : Friday, July 6, 2007.
% Author : Olivier Boudeville (olivier.boudeville@esperide.com).
% Released under GPL.

% Provides most classical constructs : new/delete operators, remote method 
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
% The hashtable type, defined in hashtable.erl, is used at all levels : 
% per-instance (for the attribute table), per-class (for the so-called virtual
% table), per-node (for the class manager).

% When an exported function is called as a method (i.e. it is listed in 
% the wooper_method_export variable, see below) the list of parameters
% being received is prefixed with the instance state (a bit like 'self' in
% Python) : A ! { aMethod, [1,2] } results in the calling of the 'aMethod'
% function defined in the class module of A (exported thanks to
% wooper_method_export) with parameters automatically given to that function
% being : 'CurrentStateOfA, 1, 2' instead of '1, 2', with 
% CurrentStateOfA being the A state variable automatically kept in the instance
% main loop.
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2
% (it is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is
% called with only two parameters specified (state not being included).


% The usual content of the '-export([XXX]).' clause in a class module should be
% dispatched in:
%
%   '-define(wooper_method_export,YYY).', to declare methods, ex : 
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
%   '-define(wooper_helper_export,new/p,construct/p+1,toString/1).'
% Ex: '-define(wooper_helper_export,new/2,construct/3,toString/1).' to 
% declare the appropriate construction-related functions (new and construct),
% p being the number of parameters defined in the wooper_construct_attributes
% variable. Only the relevant 'construct' function has to be actually 
% defined by the developer: new is automatically defined appropriately
% (see in this file). toString is optional but proved to be often convenient
% for debugging
%
%	'-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
% functions, that are not methods
    

% Shared code. 
%
% All WOOPER classes should mention their superclasses and their WOOPER 
% exports before the WOOPER header is included.

% Example :
% -module(class_Cat).
% -define(superclasses,[class_Mammal,class_ViviparousBeing]).
% -define(wooper_export,hasWhiskers/1,canEat/2).
% -include("wooper_class_root.hrl").
% [...]



% Implementation notes.


% Records the state of an instance.
% Module is the Erlang module the class is mapped to.
% This is the class-specific object state, each instance of this class
% will have its own state_holder, quite similar to the 'C++' this pointer.
% Constant data (ex : the virtual table) are referenced by each class instance,
% they are not duplicated (pointer to a virtual table shared by all class
% instances rather than deep copy).
% The attribut table (a hashtable) records all the data members of a given
% instance, including all the inherited ones. 
-record( state_holder, {
		virtual_table,
		attribute_table
	}).



% A list could be managed that would allow to discriminate the methods from
% the other exported functions. As macros cannot be substitued in strings
% it would probably force the developer to list them twice.

% The class name, as mapped to a module.
-define(className,?MODULE).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of
% actual methods)
-define(WooperAttributeCountUpperBound,5).


% The name of the registered process that keeps per-class method hashtables :
-define(WooperClassManagerName,wooper_class_manager).


% WOOPER internal functions.

% Comment/uncomment to activate debug mode :
-define(wooper_debug,).


% On debug mode, methods will have to return an atom to ensure they
% respect the right format :
-ifdef(wooper_debug).

	% These methods are defined for all classes :
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,
		is_wooper_debug/0,wooper_debug_listen/3,wooper_display_state/1,
		wooper_construct_and_run/1).

	% Actual '-export' clause, created from statically WOOPER-defined methods 
	% (WooperBaseMethods) and from user-supplied class-specific exports
	% (wooper_export).
	%-ifdef(wooper_export).
	%	-export([?wooper_export,?WooperBaseMethods]).
	%-else.
	%	-export([?WooperBaseMethods]).
	%-endif.
	
	-export([?WooperBaseMethods]).
	
	-ifdef(wooper_method_export).
		-export([?wooper_method_export]).
	-endif.
	
	% Must be defined, but an error message at their call should be clearer :
	-ifdef(wooper_helper_export).
		-export([?wooper_helper_export]).
	-endif.
	

	is_wooper_debug() ->
		true.
	
	-define(wooper_return_state_result(State,Result),{result,State,Result}).
	-define(wooper_return_state_only(State)         ,{result,State}).

	% Helper function to test requests.
	wooper_debug_listen(Pid,Action,Arguments) ->
		Pid ! {Action,Arguments,self()},
		receive
	
			Anything ->
				io:format("Answer to call to ~w with arguments ~w : ~w~n",
					[Action,Arguments,Anything])
	
		end.
	
-else.

	% Actual '-export' clause, created from statically WOOPER-defined methods 
	% (WooperBaseMethods) and from user-supplied class-specific exports
	% (wooper_export).
	-ifdef(wooper_export).
		-export([?wooper_export,?WooperBaseMethods]).
	-else.
		-export([?WooperBaseMethods]).
	-endif.

	is_wooper_debug() ->
		false.
	
	% These methods are defined for all classes :
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,
		is_wooper_debug/0,wooper_display_state/1,wooper_construct_and_run/1).

	-define(wooper_return_state_result(State,Result),{State,Result}).
	-define(wooper_return_state_only(State),        State).
	
-endif.



% Static method which returns the name of the class.
get_class_name() ->
	?className.

% Static method which returns the list of the superclasses for that class.
get_superclasses() ->
	?superclasses.


% Method that returns the classname of the instance.
get_class_name(State) ->
	?wooper_return_state_result(State,?className).

% Method that returns the superclasses of the instance.
get_superclasses(State) ->
	?wooper_return_state_result(State,?superclasses).



% Spawns a new instance for this class, using specified parameters to
% construct it.
new(?wooper_construct_attributes) ->
	% Double-list : list with a list in it.
	spawn(?MODULE,wooper_construct_and_run, [[?wooper_construct_attributes]] ).
	

% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run(ParameterList) ->
	% ?MODULE must be specified, otherwise apply/2 returns : {badfun,construct}
	% despite construct is exported with the right arity (do not know why...)
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound)
	},
	wooper_main_loop(
		apply(?MODULE,construct,[BlankTable|ParameterList]) ).
	


% The two following functions have been commented out as their macro 
% counterparts should be faster and do not lead to warnings when they are 
% not used by the class.


% Sets specified attribute of the instance to the specified value, thanks to
% specified state.
% Returns an updated state.
% See also : the similarly named macro.
%setAttribute(State,AttributeName,AttributeValue) ->
%	#state_holder{
%		virtual_table   = State#state_holder.virtual_table,
%		attribute_table = hashtable:addEntry(
%			AttributeName,
%			AttributeValue,
%			State#state_holder.attribute_table )
%	}.	
%

% Sets specified attribute of the instance to the specified value, thanks to
% specified state.
% Returns an updated state.
% See also : the similarly named function.
-define(setAttribute(State,AttributeName,AttributeValue),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntry(
			AttributeName,
			AttributeValue,
			State#state_holder.attribute_table )
	}
).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName, ClassName }'.
% See also : the similarly named macro.
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
% See also : the similarly named function.
-define(getAttribute(State,AttributeName),
	case hashtable:lookupEntry( AttributeName,
			State#state_holder.attribute_table ) of
		
		undefined ->
			{ attribute_not_found, AttributeName, get_class_name() } ;
			
		{value,Value} ->
			Value
			
	end			
).


% Returns the Wooper Class Manager.
% If if it is already running, find it and returns its atom, otherwise launch
% it and returns its PID (both can be used identically to send it messages).
wooper_get_class_manager() ->
	case lists:member( ?WooperClassManagerName, registered() ) of
		true ->
			?WooperClassManagerName;
		
		_ ->
			spawn(?WooperClassManagerName,start,[])
	end.		


% Most functions below could be encapsulated in a WOOPER-dedicated module
% (in a .erl instead of a .hrl), but in this case calls may be less efficient.

% Returns a textual representation of the specified state.
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
		io_lib:format( "Instance of ~s with ~B attribute(s) :~n",
			[get_class_name(),length(Attributes)]),
		Attributes).	


-ifdef(wooper_debug).

wooper_display_state(State) ->
	io:format("State of ~w : ~s~n", [self(),wooper_state_toString(State)]).
	
-else.
	
wooper_display_state(_) ->
	debug_not_activated.
	
-endif.
	

% Waits for incoming requests and serves them.
wooper_main_loop(State) ->
	wooper_display_state(State),
		
	receive
			
		% Request with response :
		% Server PID could be sent back as well to discriminate 
		% received answers on the client side.
		{ MethodAtom, ArgumentList, SenderPID } 
				when is_pid(SenderPID) and is_list(ArgumentList) ->
			{ NewState, Result } = 
				wooper_execute_method(MethodAtom, State, ArgumentList ), 
			%SenderPID ! { self(), Result }
			SenderPID ! Result,
			wooper_main_loop(NewState);
		
		% Auto-wrapping single arguments implies putting lists between
		% double-brackets		
		{ MethodAtom, Argument, SenderPID } when is_pid(SenderPID) ->
			{ NewState, Result } = 
				wooper_execute_method(MethodAtom, State, [ Argument ] ), 
			%SenderPID ! { self(), Result }
			SenderPID ! Result,
			wooper_main_loop(NewState);


		% Oneway calls (no client PID sent, no answer sent back) :
		% (either this method does not return anything, or the sender is not
		% interested in the result)
		{ MethodAtom, ArgumentList } when is_list(ArgumentList) ->
			% Any result would be ignored, only the update state is kept :
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, ArgumentList ),
			wooper_main_loop(NewState);
			
		{ MethodAtom, Argument } ->
			% Any result would be ignored, only the update state is kept :
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, [ Argument ] ),
			wooper_main_loop(NewState);
			
		MethodAtom when is_atom(MethodAtom) ->
			{ NewState, _ } = wooper_execute_method( 
				MethodAtom, State, [] ),
			wooper_main_loop(NewState)

	end,
	io:format( "wooper_main_loop exited.~n" ).

	
	
% Returns the virtual table corresponding to this class.
wooper_retrieve_virtual_table() ->
	% For per-instance virtual table : wooper_create_method_table_for(?MODULE).
	wooper_get_class_manager() ! { get_table, ?MODULE, self() },
	receive
	
		{ virtual_table, ?MODULE, Table } ->
			Table
	
	end.
	
	
	
	
% Looks-up specified method (Method/Arity, ex : toString/0) to be found 
% in heritance tree and returns either { methodFound, Module } with 
% Module corresponding to the
% class that implements that method, or an error.
% Note : uses the pre-built virtual table for this class.
wooper_lookupMethod(State,MethodAtom,Arity) ->
	hashtable:lookupEntry( {MethodAtom,Arity},
		State#state_holder.virtual_table).



% Following code is duplicated because no '-ifdef' clause can be defined in
% case clauses :


-ifdef(wooper_debug).	
 
 
% Executes the specified method, designated by its atom, with specified 
% instance state and parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom 'method_not_found'
% is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {result,Result} is returned, with R being 
% the actual result of the method call, with an updated state.
% Finally, if the method does not return any result, the atom
% 'method_returns_void' is returns, which allows a client that sent his
% PID to be warned it is useless, as no answer should be expected.
wooper_execute_method(MethodAtom,State,Parameters) ->	
	%io:format("wooper_execute_method : executing ~s:~s(~w).~n",
	%	[ ?MODULE, MethodAtom, Parameters ]), 	
	% +1 : take into account the State additional parameter :
	case wooper_lookupMethod(State, MethodAtom,length(Parameters)+1) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations :
			case apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Matched expressions have to be reordered depending on the
				% debug mode : 


				% Void method (no result returned, only a state) :
				% ?wooper_return_state_only :
				{result,NewState} ->  
					{NewState,method_returns_void};
				
				% Method returning a result (and a state of course) :
				% ?wooper_return_state_result :
				{result,NewState,Result} ->  			
					{NewState,{result,Result}};
				

				
				{'EXIT',ErrorTerm} ->
					{State,
						{method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, ErrorTerm} } ;

				{'EXIT',Pid,ErrorTerm} ->
					{State,
						{method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, [Pid,ErrorTerm]} } ;

				Other ->
					{State, 
						{method_faulty_return, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, Other} }	
						
			end;
			
		undefined ->
			% Method name and arity returned as separate tuple elements, as
			% if in a single string ("M/A"), the result is displayed as a list :
			{State, {method_not_found, ?MODULE, MethodAtom,
				length(Parameters)+1 } }
				
					
		% No other term can be returned.
		
	end.		



-else.

% Not in debug mode here :


% Executes the specified method, designated by its atom, with specified 
% instance state and parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom 'method_not_found'
% is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {result,Result} is returned, with R being 
% the actual result of the method call, with an updated state.
% Finally, if the method does not return any result, the atom
% 'method_returns_void' is returns, which allows a client that sent his
% PID to be warned it is useless, as no answer should be expected.
wooper_execute_method(MethodAtom,State,Parameters) ->
	% +1 : take into account the State additional parameter :
	case wooper_lookupMethod(State,MethodAtom,length(Parameters)+1) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations :
			case apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Matched expressions have to be reordered depending on the
				% debug mode : 


				
				{'EXIT',ErrorTerm} ->
					{State,
						{method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, ErrorTerm} } ;

				{'EXIT',Pid,ErrorTerm} ->
					{State,
						{method_failed, self(), ?MODULE, MethodAtom,
							length(Parameters)+1, 
							Parameters, [Pid,ErrorTerm]} } ;


				{NewState,Result} ->  
					{NewState,{result,Result}};
							
							
				% Void method (no result returned, only a state) :
				NewState ->  
					{NewState,method_returns_void}
						
			end;
			
		undefined ->
			% Method name and arity returned as separate tuple elements, as
			% if in a single string ("M/A"), the result is displayed as a list :
			{State, {method_not_found, ?MODULE, MethodAtom,
				length(Parameters)+1 } }
				
					
		% No other term can be returned.
		
	end.	
	
		
-endif.
