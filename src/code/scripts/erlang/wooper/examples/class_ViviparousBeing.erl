-module(class_ViviparousBeing).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).

% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_attributes,).

% Construction-related exported operators:
-define(wooper_construct_export,new/0,construct/1).

% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export,getMeanChildrenCount/1,getBirthGivenCount/1,
	giveBirth/2).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Viviparous being (parameter-less constructor).
construct(State) ->
	?setAttribute(State,birth_given_count,0).
	
	
	
	
% Method implementations.


% Let's say an average means something here:
% (this is a static method, as it does not depend on a state)
getMeanChildrenCount(State) ->
	?wooper_return_state_result( State, 4 ).
	
	
% Returns the number of times this viviparous being gave birth:	
getBirthGivenCount(State) ->
	?wooper_return_state_result( State, 
		?getAttribute(State,birth_given_count) ).
		
		
% Increase the number of times this viviparous being gave birth:	
giveBirth(State,NumberOfNewChildren) ->
	?wooper_return_state_only( ?setAttribute(State,birth_given_count, 
		?getAttribute(State,birth_given_count) + NumberOfNewChildren ) ).
		
	
