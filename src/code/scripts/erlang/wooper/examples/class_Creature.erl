-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'):
-define(wooper_construct_parameters,Age,Gender).

% Construction-related exported operators:
-define(wooper_construct_export,new/2,construct/3,toString/1).


% Declarations of class-specific methods (besides inherited ones).
% isHotBlooded/1 and canEat/2 are abstract here, hence not mentioned:
-define(wooper_method_export,getAge/1,setAge/2,declareBirthday/1,getGender/1).


% Non-method exported functions:
-export([example_fun/0]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").
	

% Constructs a new Creature.
construct(State,?wooper_construct_parameters) ->
	% No mother class.
	% Sanity checks could be implemented here.
	AgeState = ?setAttribute(State,age,Age),
	?setAttribute(AgeState,gender,Gender).
	

% This looks like a method, but it is not (returning only a string):	
toString(State) ->
	hashtable:toString( State#state_holder.attribute_table ).

	

% Method implementations.


% Returns the age of this creature.
getAge(State) ->
	?wooper_return_state_result(State,?getAttr(age)).
	
	
% Sets the age of this creature.
setAge(State,NewAge) ->
	?wooper_return_state_only(?setAttribute(State,age,NewAge)).


% Increments the age of this creature.
declareBirthday(State) ->
	?wooper_return_state_only(
		?setAttribute(State,age,?getAttr(age)+1)).
	
	
% Returns the gender of this creature.
getGender(State) ->
	?wooper_return_state_result(State,?getAttr(gender)).


% Just to show it can exist:	
example_fun() ->
	ok.
	
	
