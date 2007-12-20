-module(class_OvoviviparousBeing).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).

% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,).

% Construction-related exported operators:
-define(wooper_construct_export,new/0,new_link/0,construct/1).

% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export,getMeanEggsCount/1,getEggsLaidCount/1,
	layEggs/2).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Ovoviviparous being (parameter-less constructor).
construct(State) ->
	?setAttribute(State,eggs_count,0).


	
% Method implementations.


% Let's say an average means something here:
% (this is a static method, as it does not depend on a state)
getMeanEggsCount(State) ->
	?wooper_return_state_result( State, 1000 ).
	

% Returns the number of eggs this ovoviviparous laid:	
getEggsLaidCount(State) ->
	?wooper_return_state_result( State, 
		?getAttribute(State,eggs_count) ).
		
		
% Increase the number of eggs this ovoviviparous laid:	
layEggs(State,NumberOfNewEggs) ->
	?wooper_return_state_only( ?setAttribute(State,eggs_count, 
		?getAttribute(State,eggs_count) + NumberOfNewEggs ) ).
		
