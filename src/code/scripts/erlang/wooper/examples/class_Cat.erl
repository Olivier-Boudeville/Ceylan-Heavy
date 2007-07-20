-module(class_Cat).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[class_Mammal,class_ViviparousBeing]).


% Parameters taken by the constructor. They are here the ones of the mammal
% mother class (the viviparous being class do not need any parameter) plus 
% fur color (class-specific data needing to be set in the constructor) :
-define(wooper_construct_attributes,Age,Gender,FurColor).

-define(wooper_export,new/4,getTeatCount/1,canEat/2).


% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").


	
% Cats are carnivorous though :	
canEat(_,soup) ->	
	true;
	
canEat(_,chocolate) ->	
	true;
	
canEat(_,croquette) ->	
	true;

canEat(_,_) ->
	false.	



% Creates a new process that will start by constructing itself.	
new(FurColor) ->
	% spawn if needed wooper_class_manager
	% For the moment, the instance state is created from the launching
	% process, it allows to avoid the need for the developer to define
	% another class-specific intermediary function to call the constructor
	% (with class-specific parameters) and then to run the main loop.
	BlankState = wooper_create_blank_state(),
	
	% Spawns a new instance :
	spawn(?MODULE,[construct(BlankState,FurColor) ])


% Actual constructor, set the instance initial state.
% Must call the constructor of each direct superclass with the 
% relevant parameters before updating itself the resulting state and 
% returning it.
construct(State,Age,Sex,FurColor,WhiskerColor) ->
	% First the direct mother classes :
	MamalState = class_Mammal:construct(State,Age,Sex,FurColor),
	ViviparousMamalState = class_ViviparousBeing:construct(
		MamalState,false),
	% Then the class-specific attributes :
	setAttribute(ViviparousMamalState,whisker_color,WhiskerColor).
	
	

