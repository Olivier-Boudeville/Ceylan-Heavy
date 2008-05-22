% Describable class, base of all instances able to output a textual description
% of their state.
-module(class_Describable).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Description).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).

% Method declarations.
-define(wooper_method_export,getDescription/1,setDescription/2).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Implementation Note:
% Being a trace emitter is not strictly needed, as it leads to useless 
% diamond-shaped multiple inheritance.

	
% Constructs a new describable instance, based on a record of an in-world
% location. 
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	
	?setAttribute( State, description, Description ).
	
	
% Overriden destructor.
delete(State) ->
	% Class-specific actions:
	% Then call the direct mother class counterparts and allow chaining:
	State.
	
	
	

% Methods section.

	
% Returns the description of this Describable.
% (request)
getDescription(State) ->
	?wooper_return_state_result( State, ?getAttr(description) ).


% Sets the description of this Describable.
% (oneway)
setDescription(State,NewDescription) ->
	?wooper_return_state_only( ?setAttribute( State, description,
		NewDescription) ).
	
		
% Section for helper functions (not methods).

	
