% WOOPER : Wrapper for OOP in ERlang.

% See documentation at :
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date : Friday, July 6, 2007.
% Author : Olivier Boudeville (olivier.boudeville@esperide.com).
% Released as GPL.

% Provides most classical constructs : new/delete operators, remote method 
% invocation (RMI), polymorphism and multiple inheritance, all with state
% management and in a quite efficient way (i.e. no significantly faster 
% approach in Erlang could be imagined by the author).

% Instances are created thanks to the new operator, which calls the relevant
% constructors ('construct' function).
% Abstract classes need only to define their 'construct' function, as no new
% operator can apply.


% All exported functions by a class module becomes automatically methods 
% callable remotely.

% When an exported function is called as a method, the list of parameters
% being received is prefixed with the instance state (a bit like 'self' in
% Python) : A ! { aMethod, [1,2] } results in the calling of the 'aMethod'
% function defined in the class module of A (that must have exported it) with
% parameters being [ CurrentStateOfA, 1, 2 ] instead of [ 1, 2], with 
% CurrentStateOfA being the A state variable kept in main loop.
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2
% (it is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is
% called with only two parameters specified (State not being included).


% The '-export([XXX]).' clause in a class module should be replaced by :
% '-define(wooper_export,XXX).'
%
% Ex : '-export([a/1,b/2]).' becomes '-define(wooper_export,a/1,b/2).'
% Zero arity is not possible since there is at least the implicit 'State'
% argument.

% Note : one should not forget, when overloading a method M/A, to specify it in
% wooper_export, otherwise its closest ancestor method will be called instead.
% In this case a warning is issued at compilation of the child class:
% 'Warning: function M/A is unused.'


% Implementation notes :

% A class C is mapped to an Erlang module, preferably named 'class_C'.
% An active object is mapped to an Erlang process.
% Methods support Remote Invocation Calls, with no syntax difference.
% These calls are mapped to Erlang messages.
% Inheritance is implemented thanks to a per-class method virtual table,
% including the locally-defined ones and all the inherited ones.
% Instance state is maintained thanks to a per-instance attribute table,
% storing all its attributes, including all the inherited ones.
% These two tables use the hashtable type defined in hashtable.erl.

% Instance creations are a bit longer due to the creation from scratch of 
% the method hashtable, and instance sizes are increased (each has it own
% virtual table).
% An alternate solution would be to share the virtual table, notably thanks
% to a per-class registered process. It would not be called on method look-up,
% it would just give a reference to the shared method table to all instances,
% at their creation.

 

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


% These methods are defined for all classes :
%-define(WooperBaseMethods,get_class_name/0,get_superclasses/0).
% For debugging purpose :
-define(WooperBaseMethods,get_class_name/0,get_superclasses/0,
	wooper_execute_method/3,wooper_main_loop/1).

% A list could be managed that would allow to discriminate the methods from
% the other exported functions. As macros cannot be substitued in strings
% it would probably force the developer to list them twice.

% The class name, as mapped to a module.
-define(className,?MODULE).

% Actual '-export' clause, created from statically WOOPER-defined methods 
% (WooperBaseMethods) and from user-supplied class-specific exports
% (wooper_export).
-ifdef(wooper_export).
  -export([?wooper_export,?WooperBaseMethods]).
-else.
  -export([?WooperBaseMethods]).
-endif.


% Static method which returns the name of the class.
get_class_name() ->
	?className.


% Static method which returns the list of the superclasses for that class.
get_superclasses() ->
	?superclasses.



% Approximate average method count for a given class, including inherited ones.
% (ideally should be slightly above the maximum number of actual methods)
-define(WooperMethodCountUpperBound,5).

% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of
% actual methods)
-define(WooperAttributeCountUpperBound,5).



% WOOPER internal functions.


% Spawns a new instance for this class, using specified parameters to
% construct it.
new(?wooper_construct_attributes) ->
	spawn(?MODULE, construct,
			[wooper_create_blank_state(),?wooper_construct_attributes]).




% Sets specified attribute of the instance to the specified value, thanks to
% specified state.
% Returns an updated state.
% FIXME should be a macro 
setAttribute(State,AttributeName,AttributeValue) ->
	#state_holder{
		virtual_table   = State#state_holder.virtual_table,
		attribute_table = hashtable:addEntry(
			AttributeName,
			AttributeValue,
			State#state_holder.attribute_table )
			
	}.	



% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName, ClassName }'.
% FIXME should be a macro 
getAttribute(State,AttributeName) ->
	case hashtable:lookupEntry( AttributeName,
			State#state_holder.attribute_table ) of
		
		undefined ->
			{ attribute_not_found, AttributeName, get_class_name() } ;
			
		{value,Value} ->
			Value
			
	end.			

wooper_create_method_table() ->
	wooper_create_method_table_for(?MODULE).



% FIXME next should be in .erl maybe


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


% Waits for incoming requests and serves them.
wooper_main_loop(State) ->

	io:format( "State of ~w : ~s~n", [self(),wooper_state_toString(State)] ),
	
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

	end.

	
wooper_create_blank_state() ->
	#state_holder{
		virtual_table   = wooper_create_method_table(),
		attribute_table = hashtable:new(?WooperAttributeCountUpperBound)
	}.


% Returns a Hashtable appropriate for method look-up, for the specified module.
% Constructors (construct) could be ignored.
wooper_create_local_method_table_for(Module) ->
	lists:foldl(
		fun(FunNameArityPair,HashTable) ->
			hashtable:addEntry(FunNameArityPair,Module,HashTable)
		end,
		hashtable:new(?WooperMethodCountUpperBound),
		Module:module_info(exports)).


% Updates specified virtual table with the method of specified module
% (i.e. precomputes the virtual table for the related class)
% In case of key collision, the values specified in HashTable have
% priority over the ones relative to Module. Hence 
wooper_update_method_table_with(Module,HashTable) ->
	hashtable:merge(HashTable,wooper_create_method_table_for(Module)).
	

wooper_create_method_table_for(TargetModule) ->
	lists:foldl(
		fun(Module,HashTable) ->
			wooper_update_method_table_with(Module,HashTable)
		end,		
		wooper_create_local_method_table_for(TargetModule),
		apply(TargetModule,get_superclasses,[])).

	
% Returns the virtual table corresponding to this class.
wooper_get_virtual_table() ->
	wooper_create_method_table().
	
	
% Looks-up specified method (Method/Arity, ex : toString/0) to be found 
% in heritance tree and returns either { methodFound, Module } with 
% Module corresponding to the
% class that implements that method, or an error.
% Note : uses the pre-built virtual table for this class.
wooper_lookupMethod(MethodAtom,Arity) ->
	hashtable:lookupEntry({MethodAtom,Arity},wooper_get_virtual_table()).

 
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
	case wooper_lookupMethod(MethodAtom,length(Parameters)+1) of
	
		{ value, LocatedModule } -> 
			% The 'return' atom is a safety guard against incorrect method
			% implementations :
			case apply(LocatedModule,MethodAtom,[State|Parameters]) of
			
				% Void method (nothing returned) :
				{return,NewState} ->  
					{NewState,method_returns_void};

				{return,NewState,Result} ->  
					{NewState,{result,Result}};
				
				{'EXIT',ErrorTerm} ->
					{State,
						{method_failed, ?MODULE, MethodAtom,
							length(Parameters)+1, ErrorTerm} } ;
					
				Other ->
					{State, 
						{method_faulty_return, ?MODULE, MethodAtom,
							length(Parameters)+1, Other} }
						
			end;
			
		undefined ->
			{State, {method_not_found, ?MODULE, MethodAtom,
				length(Parameters)+1 } }
				
					
		% No other term can be returned.
		
	end.		

