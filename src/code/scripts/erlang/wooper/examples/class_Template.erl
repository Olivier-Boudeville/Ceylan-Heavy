-module(class_MYCLASS).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_attributes,ATTR1,ATTR2).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/P,construct/P+1).

% Method declarations.
-define(wooper_method_export,M1/A1,M2/A2).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new MYCLASS.
construct(State,?wooper_construct_attributes) ->
	% Note: when constructing the initial state thanks to intermediate ones,
	% always build newer state from the result of last update. Reusing an
	% already updated state will loose all the changes made in-between, ex:
	% S1 = f(State), S2 = g(S1), 
	% WRONG: S3 = h(S1)  [S2 lost]
	% RIGHT: S3 = h(S2)  [S2 used as expected]
	% (useful with multiple inheritance where functions are class_XXX:construct)
	
	% First the direct mother classes:
	
	% Then the class-specific attributes:
	

M1(State,ARG1) ->

	?wooper_return_state_result( A_STATE, A_RESULT ).


M2(State,ARG1,ARG2) ->

	?wooper_return_state_only( A_STATE ).

	

