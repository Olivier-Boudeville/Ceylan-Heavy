-module(class_Cat).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Mammal,class_ViviparousBeing]).


% Parameters taken by the constructor ('construct'). 
% They are here the ones of the Mammal mother class (the viviparous being 
% constructor does not need any parameter) plus whisker color.
% These are class-specific data needing to be set in the constructor:
-define(wooper_construct_attributes,Age,Gender,FurColor,WhiskerColor).

% Construction-related exported operators:
-define(wooper_construct_export,new/4,construct/5).

% Method declarations.
-define(wooper_method_export,getTeatCount/1,canEat/2,getWhiskerColor/1).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new Cat.
construct(State,?wooper_construct_attributes) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),
	
	% Then the class-specific attributes:
	?setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).
	

% No guarantee on biological fidelity:	
getTeatCount(State) ->
	?wooper_return_state_result( State, 6 ).

	
% Cats are supposed carnivorous though:
canEat(State,soup) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,chocolate) ->	
	?wooper_return_state_result( State, true );
	
canEat(State,croquette) ->	
	?wooper_return_state_result( State, true );

canEat(State,meat) ->	
	?wooper_return_state_result( State, true );

canEat(State,_) ->
	?wooper_return_state_result( State, false ).


getWhiskerColor(State)->
	?wooper_return_state_result( State, ?getAttr(whisker_color) ).


