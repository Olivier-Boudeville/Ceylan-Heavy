-module(class_Creature).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[]).

% isHotBlooded/1 and canEat/2 are abstract hence not mentioned here :
-define(wooper_export,construct/3,getAge/1,setAge/2,declareBirthday/1,
	getGender/1).


% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").


% Constructs a new Creature.
construct(State,Age,Gender) ->
	% No mother class.
	% Sanity checks could be implemented here.
	AgeState = setAttribute(State,age,Age),
	setAttribute(AgeState,gender,Gender),
	

% Method implementations.

% Returns the age of this creature.
getAge(State) ->
	{return,State,getAttribute(State,age)}.
	
	
% Sets the age of this creature.
setAge(State,NewAge) ->
	{return,setAttribute(State,age,NewAge)}.


% Increments the age of this creature.
declareBirthday(State) ->
	{return,setAttribute(State,age,getAttribute(State,age)+1)}.
	
	
% Returns the gender of this creature.
getGender(State) ->
	{return,State,getAttribute(State,gender)}.
	

