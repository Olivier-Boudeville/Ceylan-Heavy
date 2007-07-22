-module(class_Mammal).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Creature]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the mother class (creature) plus fur color:
-define(wooper_construct_parameters,Age,Gender,FurColor).

% Life cycle-related exported operators:
-define(wooper_construct_export,new/3,construct/4,delete/1).

% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export,isHotBlooded/1,getFurColor/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Mammal.
construct(State,?wooper_construct_parameters) ->
	CreatureState = class_Creature:construct(State,Age,Gender),
	?setAttribute(CreatureState,fur_color,FurColor).
	
	
	
	
% Method implementations.


% All mammals are hot-blooded:
isHotBlooded(State) ->
	?wooper_return_state_result(State,true).


% Attribute names could be defined in '-define().' header (.hrl) clauses,
% to ensure consistency.
getFurColor(State) ->
	?wooper_return_state_result( State, ?getAttribute(State,fur_color) ).
	

% Overriding default destructor :	
% State should be returned, and destructors should be called in leaf-to-root
% order in inheritance tree.
delete(State) ->
	io:format( "Destructing a Mammal (overriden destructor).~n" ),
	State.
	
	
