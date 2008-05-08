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
%    '-define(wooper_method_export,YYY).', to declare methods, ex: 
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
% '-define(wooper_construct_export,new/p,new_link/p,construct/p+1,toString/1).'
% Ex:
% '-define(wooper_construct_export,new/2,new_link/2,construct/3,toString/1).' 
% to declare the appropriate construction-related functions (the two 'new', and
% 'construct'), p being the number of parameters defined in the
% wooper_construct_parameters variable.
% Only the relevant 'construct' function has to be actually defined by the
% developer: new and new_lib are automatically defined appropriately
% (see in this file). toString is optional but proved to be often convenient
% for debugging method implementations
%
%    '-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
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
% -define(wooper_construct_export,new/3,new_link/3,construct/4).
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
-define(wooper_debug,).


% On debug mode, methods will have to return an atom to ensure they
% respect the right format:
-ifdef(wooper_debug).

	% These methods/functions are defined for all classes:
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,
		executeRequest/3,executeRequest/2,executeOneway/3,executeOneway/2,
		wooper_pop_from_attribute/2,
		wooper_construct_and_run/1,wooper_construct_and_run_synchronous/2,
		is_wooper_debug/0,wooper_debug_listen/3, 
		wooper_display_state/1,wooper_display_virtual_table/1,
		wooper_display_instance/1,
		wooper_get_state_description/1,wooper_get_virtual_table_description/1,
		wooper_get_instance_description/1,wooper_display_loop_state/1).
	
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
		
	-define(wooper_return_state_only(State),{wooper_result,State}).


	wooper_display_loop_state(State) ->
		wooper_display_state(State).

	
-else.

	% Not in debug mode here:

	
	% These methods/functions are defined for all classes:
	-define(WooperBaseMethods,get_class_name/0,get_class_name/1,
		get_superclasses/0,get_superclasses/1,
		executeRequest/3,executeRequest/2,executeOneway/3,executeOneway/2,
		wooper_pop_from_attribute/2,
		wooper_construct_and_run/1,wooper_construct_and_run_synchronous/2,
		is_wooper_debug/0,wooper_debug_listen/3,
		wooper_display_state/1,wooper_display_virtual_table/1,
		wooper_display_instance/1,wooper_display_loop_state/1).

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
	-define(wooper_return_state_only(State), State).
	
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
% Returns the PID of the newly created instance.
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
new(?wooper_construct_parameters) ->
	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]), 
	% Double-list: list with a list in it.
	spawn(?MODULE, wooper_construct_and_run,
		[[?wooper_construct_parameters]] ).
	

% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created instance.
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
new_link(?wooper_construct_parameters) ->
	% Double-list: list with a list in it.
	spawn_link(?MODULE, wooper_construct_and_run,
		[[?wooper_construct_parameters]] ).


% Spawns a new instance for this class, using specified parameters to
% construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
synchronous_new(?wooper_construct_parameters) ->
	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]), 
	% Double-list: list with a list in it, and ++ used to allow for empty
	% parameter list.
	SpawnedPid = spawn(?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
		
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.
	

% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created instance.
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
synchronous_new_link(?wooper_construct_parameters) ->
	% Double-list: list with a list in it, and ++ used to allow for empty
	% parameter list..
	SpawnedPid= spawn_link(?MODULE, wooper_construct_and_run_synchronous,
		[ [?wooper_construct_parameters], self() ] ),
	
	% Blocks until the spawned process answers:	
	receive	
		
		{spawn_successful,SpawnedPid} ->
			SpawnedPid
	
	end.


% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run(ParameterList) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	wooper_main_loop(
		apply(?MODULE,construct,[BlankTable|ParameterList]) ).



% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent. 
wooper_construct_and_run_synchronous(ParameterList,SpawnerPid) ->
	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{
		virtual_table   = wooper_retrieve_virtual_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound),
		request_sender  = undefined
	},
	ConstructState = apply(?MODULE,construct,[BlankTable|ParameterList]),
	SpawnerPid ! { spawn_successful, self() },
	wooper_main_loop( ConstructState ).




% State management section.



% Sets specified attribute of the instance to the specified value, based from
% specified state.
% Returns an updated state.
% Always succeeds.
% See also: the setAttributes macro to set more than one attribute at a time.
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


% Sets a list of attribute/value pairs in specified state.
% The expected parameter is a list of pairs (2-element tuples), each pair 
% containing in first position the attribute name and in second one the 
% attribute value.
% Returns an updated state.
% Always succeeds.
% See also: the setAttribute macro.
-define(setAttributes(State,ListOfAttributePairs),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntries(
			ListOfAttributePairs,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).
	

% Tells whether specified attribute exists, returns true or false.
-define(hasAttribute(State,AttributeName),
	hashtable:hasEntry( AttributeName, State#state_holder.attribute_table ) ).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise triggers a case clause crash.
% See also: getAttr/1.
-define(getAttribute(State,AttributeName),
	hashtable:getEntry( AttributeName, State#state_holder.attribute_table ) ).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise triggers a case clause crash.
% Beware to the implicit use of the 'State' variable: in some cases other
% states should be used. 
% See the getAttribute/2 macro.
-define(getAttr(AttributeName),?getAttribute(State,AttributeName)).


% Returns an updated state not having anymore specified attribute.
% No error is triggered if the specified attribute was not existing.
-define(removeAttribute(State,AttributeName),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:removeEntry( AttributeName,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Adds specified value to specified attribute, supposed to be a number.
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
-define(addToAttribute(State,AttributeName,Value),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addToEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Substracts specified value from specified attribute, supposed to be a number.
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no substraction can be performed on the attribute value.
-define(substractFromAttribute(State,AttributeName,Value),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:substractFromEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).

 
% Returns an updated state in which specified boolean attribute is toggled:
% if true will be false, if false will be true.
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
-define(toggleAttribute(State,BooleanAttributeName),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:toggleEntry(
			BooleanAttributeName, 
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).	


% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
-define(appendToAttribute(State,AttributeName,Element),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:appendToEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).


% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
% A case clause is triggered if the attribute did not exist.
% If the element is not in the specified list, the list will not be modified.
-define(deleteFromAttribute(State,AttributeName,Element),
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:deleteFromEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ),
		request_sender  = State#state_holder.request_sender
	}
).



% Removes the head from specified attribute, supposed to be a list, and 
% returns a tuple {NewState,PoppedHead}.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{PoppedState,Head} = ?popFromAttribute(State,my_list)'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
% @note This cannot be a one-line macro, it has to be a function.
%
-define(popFromAttribute(State,AttributeName),
	wooper_pop_from_attribute(State,AttributeName)).


% Helper function for the popFromAttribute macro.
wooper_pop_from_attribute(State,AttributeName) ->
	{Head,PoppedAttributeTable} = hashtable:popFromEntry(AttributeName,
		State#state_holder.attribute_table),
	
	{ #state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = PoppedAttributeTable,
		request_sender  = State#state_holder.request_sender	},
		Head
	}.	



% Returns the sender of the request.
-define(getSender(),State#state_holder.request_sender).




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
				error_logger:error_msg( "wooper_get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand.~n"
				),
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
	error_logger:info_msg( "~s~n", [wooper_state_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_virtual_table(State) ->
	error_logger:info_msg( "~s~n", [wooper_virtual_table_toString(State)] ).


% Displays the inner state of this instance.
% This is not a method.
wooper_display_instance(State) ->
	error_logger:info_msg( "~s~n", [wooper_instance_toString(State)] ).


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
	
	
	
% WOOPER default EXIT handler.
% Can be overriden by defining or inheriting the onWooperExitReceived/3 method.	
wooper_default_exit_handler(State,Pid,ExitType) ->
	io:format( "WOOPER default EXIT handler ignored signal '~w' from ~w.~n",
		[ExitType,Pid] ),
	?wooper_return_state_only(State).	



% Waits for incoming requests and serves them.
wooper_main_loop(State) ->
	
	% Comment-out to avoid the state display prior to each method call:
	%wooper_display_loop_state(State),
		
	receive
			
		% Requests with response:
		
		% Server PID could be sent back as well to discriminate 
		% received answers on the client side.
		{ MethodAtom, ArgumentList, SenderPID } 
				when is_pid(SenderPID) and is_list(ArgumentList) ->
			SenderAwareState = State#state_holder{request_sender=SenderPID},
			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, ArgumentList ), 
			%SenderPID ! { self(), Result }
			SenderPID ! Result,
			
			% Force a crash if server-side error detected: 
			case element(1,Result) of
			
				wooper_method_failed ->
					exit(Result) ;
				
				wooper_method_faulty_return ->
					exit(Result) ;
				
				_ ->
					ok	
			
			end, 
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
			
			% Force a crash if server-side error detected: 
			case element(1,Result) of
			
				wooper_method_failed ->
					exit(Result) ;
				
				wooper_method_faulty_return ->
					exit(Result) ;
				
				_ ->
					ok	
			
			end, 
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
		
		
		% ping is always available and cannot be overriden:
		{ ping, SenderPID } ->
			SenderPID ! {pong,self()},
			wooper_main_loop(State);
		
		% Oneway with parameters:		
		{ MethodAtom, Argument } ->
			% Any result would be ignored, only the update state is kept:
			{ NewState, _ } = wooper_execute_method( MethodAtom, State, 
				[ Argument ] ), 
			wooper_main_loop(NewState);
			
		delete ->
			case hashtable:lookupEntry( {delete,1},
					State#state_holder.virtual_table) of 
				
				undefined ->
					% Destructor not overriden, using default one:
					%io:format( "Deleting ~w (default destructor).~n", 
					%	[ self() ]),
					deleted;
				
				{ value, LocatedModule } -> 
					apply( LocatedModule, delete, [ State ] ),
					deleted
					
				
			% (do nothing, loop ended).		
			end;
		
			
		MethodAtom when is_atom(MethodAtom) ->
			{ NewState, _ } = wooper_execute_method( MethodAtom, State, [] ),
			wooper_main_loop(NewState);
		
		
		{'EXIT',Pid,ExitType} when is_pid(Pid) ->
			case hashtable:lookupEntry( {onWooperExitReceived,3},
					State#state_holder.virtual_table) of 
				
				undefined ->
					% EXIT handler not overriden, using default one:
					wooper_main_loop( wooper_default_exit_handler( State,
						Pid,ExitType ) );
				
				{ value, _ } -> 
					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule,onWooperExitReceived,...)':
					{ NewState, _ } = wooper_execute_method(
						onWooperExitReceived, State, [Pid,ExitType] ),
					wooper_main_loop(NewState)
								
			end;
			
		
		Other ->
			error_logger:warning_msg( "WOOPER ignored following message: ~w.~n",
				[Other]),
			wooper_main_loop(State)
			

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
% instance state and list of parameters.
% If the method is not found (either in the class module or in its
% ancester trees), an error tuple beginning with the atom
% 'wooper_method_not_found' is returned with an unchanged state.
% If the method is found, if its execution fails, an error tuple beginning 
% with the atom 'wooper_method_failed' is returned with an unchanged state.
% If it does not fail but returns an unexpected result (i.e. not a tuple 
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
% If its execution succeeds, then {wooper_result,Result} is returned, with 
% Result being the actual result of the method call, with an updated state.
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returned, which allows a client that sent his
% PID to be warned it is useless, as no answer should be expected.
% The error logs have been added, as debugging faulty oneways is more difficult:
% they cannot return any error to the caller, they can just crash and notify
% any linked or monitoring process.
wooper_execute_method(MethodAtom,State,Parameters) ->	

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]), 	
	
	% +1: take into account the State additional parameter:
	case wooper_lookupMethod( State, MethodAtom, length(Parameters)+1 ) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:

			%io:format("wooper_execute_method: executing ~s:~s(~w).~n",
			%	[ ?MODULE, MethodAtom, Parameters ]), 	
			
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
				
				% All next cases are error cases.
				% One option is to return an appropriate error term, but it 
				% is useful only for requests, as oneways send back no result. 
				% Another option is to let the faulty process crash: oneways
				% would not send more answers, but linked and monitoring 
				% processes could nevertheless by notified.
				
				% Finally, failed requests result in a log, an error answer
				% being returned, then a crash if the error is due to internal
				% reasons, whereas failed oneways result in a log then a crash,
				% similarly if the error is due to internal reasons.
				 
				{'EXIT',ErrorTerm} ->
				
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed "
								"with error term ~w for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm,
									Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
																
							% Terminates the process:	
							erlang:exit( {wooper_oneway_failed, self(), ?MODULE,
								MethodAtom, length(Parameters)+1, Parameters,
								ErrorTerm} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed "
								"with error term ~w for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm,
									Parameters] ),
								
							{State,
								{wooper_method_failed, self(), ?MODULE,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, ErrorTerm} }					
					end;
					

				{'EXIT',Pid,ErrorTerm} ->
				
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed "
								"with error term ~w and PID ~w "
								"for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm, Pid,
									Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
								
							% Terminates the process:	
							erlang:exit( {wooper_oneway_failed, self(),
								?MODULE, MethodAtom, length(Parameters)+1,
								Parameters, {Pid, ErrorTerm}} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed "
								"with error term ~w and PID ~w "
								"for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm, Pid,
									Parameters] ),
								
							{State,
								{wooper_method_failed, self(), ?MODULE,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, {Pid, ErrorTerm}} }
								
					end;
					
				
				% Not a wooper result neither an EXIT message: faulty return.
				Other ->
					
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B made a faulty return "
								"~w, parameters were ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, Other, Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
								
							% Terminates the process:	
							erlang:exit( {wooper_method_faulty_return, self(),
								?MODULE, MethodAtom, length(Parameters)+1, 
								Parameters, Other} );    
						   
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B made a faulty return "
								"~w, parameters were ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, Other, Parameters ] ),
								
							{State, 
							   	{wooper_method_faulty_return, self(),
									?MODULE, MethodAtom, length(Parameters)+1,
									Parameters, Other } }
									 
					end
						
			end;

		
		
		% Method not found:	
		undefined ->
		
			case State#state_holder.request_sender of
					
				undefined ->
		
					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple 
					% elements, as if in a single string ("M/A"), the result
					% is displayed as a list:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"oneway method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep(1000),
					
					% Terminates the process:	
					erlang:exit( {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters} );   
					 
				_ ->
						
					% This is a request, send error term and rely on
					% the calling function (wooper_main_loop) to crash:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"request method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					{State, {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters } }
				
					
			% No other term can be returned.
		
			end;
			
			
		Other ->
			error_logger:warning_msg( "WOOPER ignored following message: ~w.~n",
				[Other]),
			wooper_main_loop(State)
			
	end.



% Allows to call synchronously from the code of a given class its actual 
% overriden methods (requests, here), including from child classes.
%
% @example If in a start method of an EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% @note Stripped-down version of wooper_main_loop.
%  
executeRequest(State,RequestAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, ?MODULE ]), 	

			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ), 
		
	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result};
	
	
% Here third parameter is not a list:
executeRequest(State,RequestAtom,StandaloneArgument) -> 

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ RequestAtom, StandaloneArgument, ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [StandaloneArgument] ), 

	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result}.



% Parameter-less request.
executeRequest(State,RequestAtom) -> 

	%io:format("executeRequest/2: executing ~s() from ~s.~n", 
	%	[ RequestAtom , ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ), 

	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result}.


		
% Allows to call synchronously from the code of a given class its actual 
% overriden methods (oneways, here), including from child classes.
%
% @example If in a start method of a EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeOneway should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState}.
%
% @note Stripped-down version of wooper_main_loop.
%  
executeOneway(State,OnewayAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, ?MODULE ]), 	
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, ArgumentList ), 
	
	% Returns:	
	{wooper_result,NewState};

	
% Here third parameter is not a list:
executeOneway(State,OnewayAtom,StandaloneArgument) -> 

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ OnewayAtom, StandaloneArgument, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [StandaloneArgument] ), 

	% Returns:			
	{wooper_result,	NewState}.


% Parameter-less oneway.
executeOneway(State,OnewayAtom) -> 

	%io:format("executeOneway/2: executing ~s() from ~s.~n", 
	%	[ OnewayAtom, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [] ), 

	% Returns:			
	{wooper_result,	NewState}.





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
	case wooper_lookupMethod( State, MethodAtom, length(Parameters)+1 ) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations:
			case catch apply(LocatedModule,MethodAtom,[State|Parameters]) of

				% Matched expressions have to be reordered depending on the
				% debug mode: 
				
				{'EXIT',ErrorTerm} ->
				
					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed "
								"with error term ~w for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm,
									Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
								
							% Terminates the process:	
							erlang:exit( {wooper_oneway_failed, self(), ?MODULE,
								MethodAtom, length(Parameters)+1, Parameters,
								ErrorTerm} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed "
								"with error term ~w for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm,
									Parameters] ),
								
							{State,
								{wooper_method_failed, self(), ?MODULE,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, ErrorTerm} } 					
					end;


				{'EXIT',Pid,ErrorTerm} ->

					case State#state_holder.request_sender of
					
						undefined ->
						
							% This is a oneway, so log and crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"oneway method ~s:~s/~B failed "
								"with error term ~w and PID ~w "
								"for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm, Pid,
									Parameters] ),

							% Wait a bit as error_msg seems asynchronous:
							timer:sleep(1000),
							
							% Terminates the process:	
							erlang:exit( {wooper_oneway_failed, self(),
								?MODULE, MethodAtom, length(Parameters)+1,
								Parameters, {Pid, ErrorTerm}} );
									
						_ ->
						
							% This is a request, send error term and rely on
							% the calling function (wooper_main_loop) to crash:
							error_logger:error_msg(	"WOOPER error for PID ~w: "
								"request method ~s:~s/~B failed "
								"with error term ~w and PID ~w "
								"for parameters ~w.~n",
								[ self(), ?MODULE, MethodAtom,
									length(Parameters)+1, ErrorTerm, Pid,
									Parameters] ),
								
							{State,
								{wooper_method_failed, self(), ?MODULE,
									MethodAtom, length(Parameters)+1, 
						   			Parameters, {Pid, ErrorTerm}} }
								
					end;


				{NewState,Result} ->  
					{NewState,{wooper_result,Result}};
							
							
				% Void method (no result returned, only a state):
				% (catch-all, no faulty return can be detected here, when not
				% in debug mode)
				NewState ->  
					{NewState,wooper_method_returns_void}
						
			end;
			
			
			
		undefined ->
		
			case State#state_holder.request_sender of
					
				undefined ->
		
					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple elements,
					% as if in a single string ("M/A"), the result is displayed
					% as a list:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"oneway method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					% Wait a bit as error_msg seems asynchronous:
					timer:sleep(1000),
					
					% Terminates the process:	
					erlang:exit( {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters} );   
					 
				_ ->
						
					% This is a request, send error term and rely on
					% the calling function (wooper_main_loop) to crash:
					error_logger:error_msg(	"WOOPER error for PID ~w: "
						"request method ~s:~s/~B not found, "
						"parameters were ~w.~n",
						[ self(), ?MODULE, MethodAtom,
							length(Parameters)+1, Parameters] ),
					
					{State, {wooper_method_not_found, self(), ?MODULE,
						MethodAtom, length(Parameters)+1, Parameters } }
				
			end	
					
		% No other term can be returned.
		
	end.	




% Allows to call synchronously from the code of a given class its actual 
% overriden methods (requests, here), including from child classes.
%
% @example If in a start method of an EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeRequest should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% @note Stripped-down version of wooper_main_loop.
%  
executeRequest(State,RequestAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, ?MODULE ]), 	
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% No special checking performed in release mode:
	{ NewState, Result } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ), 
		
	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result};
	
	
	
% Here third parameter is not a list:
executeRequest(State,RequestAtom,StandaloneArgument) -> 

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ RequestAtom, StandaloneArgument, ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% No special checking performed in release mode:
	{ NewState, Result } = wooper_execute_method( RequestAtom,
		SenderAwareState, [StandaloneArgument] ), 

	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result}.



% Parameter-less request.
executeRequest(State,RequestAtom) -> 

	%io:format("executeRequest/2: executing ~s() from ~s.~n", 
	%	[ RequestAtom , ?MODULE ]),
			
	% Auto-calling method:		
	SenderAwareState = State#state_holder{request_sender=self()},
	
	% Correction checking by pattern-matching:
	{ NewState, Result } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ), 

	% Returns:			
	{wooper_result,	NewState#state_holder{request_sender=undefined}, Result}.


		
% Allows to call synchronously from the code of a given class its actual 
% overriden methods (oneways, here), including from child classes.
%
% @example If in a start method of a EngineVehicle class one wants to call the 
% (possibly overriden by, say, a class Car) startEngine method, then 
% executeOneway should be used: 'MyVehicle ! startEngine' would not be 
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
% 
% If no failure occurs, returns {wooper_result,NewState}.
%
% @note Stripped-down version of wooper_main_loop.
%  
executeOneway(State,OnewayAtom,ArgumentList) when is_list(ArgumentList) -> 

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, ?MODULE ]), 	
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, ArgumentList ), 
	
	% Returns:	
	{wooper_result,NewState};

	
% Here third parameter is not a list:
executeOneway(State,OnewayAtom,StandaloneArgument) -> 

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n", 
	%	[ OnewayAtom, StandaloneArgument, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, [StandaloneArgument] ), 

	% Returns:			
	{wooper_result,NewState}.


% Parameter-less oneway.
executeOneway(State,OnewayAtom) -> 

	%io:format("executeOneway/2: executing ~s() from ~s.~n", 
	%	[ OnewayAtom, ?MODULE ]),
			
	% No request_sender to change with oneways.
	
	% Less checking performed in release mode:
	{ NewState, wooper_method_returns_void } = wooper_execute_method(
		OnewayAtom,	State, [] ), 

	% Returns:			
	{wooper_result,NewState}.
		
		
-endif.

