-module(class_Reptile).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Creature]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the mother class (creature):
-define(wooper_construct_parameters,Age,Gender).

% Construction-related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3).

% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export,setAge/2,isHotBlooded/1,canMoult/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Reptile.
construct(State,?wooper_construct_parameters) ->
	class_Creature:construct(State,Age,Gender).
	
	
	
	
% Method implementations.


% Sets correctly the age of this Mammal (not like faulty implementation of the
% Creature mother class).
% Overriden from Creature, useful to show the use of executeOneway.
% (oneway)
setAge(State,NewAge) ->
	?wooper_return_state_only(?setAttribute(State,age,NewAge)).


% All reptiles are cold-blooded:
isHotBlooded(State) ->
	?wooper_return_state_result(State,false).


% All reptiles can moult:
canMoult(State) ->
	?wooper_return_state_result(State,true).
	
	
