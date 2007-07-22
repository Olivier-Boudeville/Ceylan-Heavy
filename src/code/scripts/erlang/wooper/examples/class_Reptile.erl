-module(class_Reptile).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Creature]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the mother class (creature):
-define(wooper_construct_parameters,Age,Gender).

% Construction-related exported operators:
-define(wooper_construct_export,new/2,construct/3).

% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export,isHotBlooded/1,canMoult/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Reptile.
construct(State,?wooper_construct_parameters) ->
	class_Creature:construct(State,Age,Gender).
	
	
	
	
% Method implementations.


% All reptiles are cold-blooded:
isHotBlooded(State) ->
	?wooper_return_state_result(State,false).


% All reptiles can moult:
canMoult(State) ->
	?wooper_return_state_result(State,true).
	
	
