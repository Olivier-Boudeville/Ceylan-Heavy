-module(class_Creature).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[]).

% isHotBlooded/1 and canEat/2 are abstract hence not mentioned here :
-define(wooper_export,new/2,construct/3,getAge/1,setAge/2,declareBirthday/1,
	getGender/1,listen/3).


-define(wooper_construct_attributes,Age,Gender).

% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").

	

% Constructs a new Creature.
construct(State,?wooper_construct_attributes) ->
	% No mother class.
	% Sanity checks could be implemented here.
	AgeState = setAttribute(State,age,Age),
	wooper_main_loop( setAttribute(AgeState,gender,Gender)).
	

listen(Pid,Action,Arguments) ->
	Pid ! {Action,Arguments,self()},
	receive
	
		Anything ->
			io:format("Answer to call to ~w with arguments ~w : ~w~n",
				 [Action,Arguments,Anything])
	
	end.
	
		
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
	

