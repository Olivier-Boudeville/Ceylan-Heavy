-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'):
-define(wooper_construct_parameters, Age, Gender ).

% Construction-related exported operators:
% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define(wooper_construct_export, new/2, new_link/2, 
	synchronous_new/2, synchronous_new_link/2,
	synchronous_timed_new/2, synchronous_timed_new_link/2,
	remote_new/3, remote_new_link/3, remote_synchronous_new/3,
	remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
	remote_synchronous_timed_new_link/3, construct/3 ).



% Declarations of class-specific methods (besides inherited ones).
% isHotBlooded/1 and canEat/2 are abstract here, hence not mentioned:
-define(wooper_method_export, getAge/1, setAge/2, declareBirthday/1, 
	getGender/1, getArbitraryNumber/1, testDirectMethodExecution/2 ).


% Non-method exported functions:
-export([ example_fun/0, toString/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").
	

% Constructs a new Creature.
construct(State,?wooper_construct_parameters) ->
	% No mother class.
	?setAttributes(State, [ {age,Age}, {gender,Gender} ] ).
	

	

% Method implementations.


% Returns the age of this creature.
getAge(State) ->
	?wooper_return_state_result(State,?getAttr(age)).
		
	
% Sets the age of this creature.
setAge(State,_NewAge) ->
	% Mother implementation chosen faulty to check override:
	?wooper_return_state_only(?setAttribute(State,age,36)).


% Increments the age of this creature.
declareBirthday(State) ->
	?wooper_return_state_only(
		?setAttribute(State,age,?getAttr(age)+1)).
	
	
% Returns the gender of this creature.
getGender(State) ->
	?wooper_return_state_result(State,?getAttr(gender)).


% Returns a class-specific arbitrary number.
% (request)
getArbitraryNumber(State) ->
	?wooper_return_state_result(State,10).


% Tests direct (synchronous) self-invocation of methods.
% (oneway).
testDirectMethodExecution(State,NewAge) ->

	io:format( "Testing executeOneway.~n" ),
	NewState = executeOneway(State,setAge,NewAge),
	
	% Not the 36 returned by this class (347 given by the test of Mammal) :
	347 = ?getAttribute(NewState,age),
	
	io:format( "Testing executeRequest.~n" ),
	% 15 from child classes (Mammal or Reptile), not 10 from here:
	{OtherState,15} = executeRequest(NewState,getArbitraryNumber,[]),

	io:format( "Direct self-invocation success.~n" ),

	?wooper_return_state_only(OtherState).



% Helper function.

% Just to show it can exist:	
example_fun() ->
	ok.
	
	
% This looks like a method, but it is not (returning only a string):	
% (function)
toString(State) ->
	hashtable:toString( State#state_holder.attribute_table ).

