% Gathering of various convenient facilities.
% See fsm_utils_test.erl for the corresponding test.

-module(fsm_utils).

% Creation date: August 30, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([create_blank_fsm_state/0,setFsmAttribute/3,getFsmAttribute/2]).




% Approximate average attribute count for a given FSM state instance.
-define(FSMAttributeCountUpperBound,5).


% Creates an attribute table appropriate to store a FSM state.
% setFsmAttribute, getFsmAttribute and getFsmAttr are to be used with these
% variables too.
create_blank_fsm_state() ->
	hashtable:new( ?FSMAttributeCountUpperBound ).


% Sets specified FSM state attribute.
setFsmAttribute(FsmState,AttributeName,AttributeValue) ->
	hashtable:addEntry( AttributeName, AttributeValue, FsmState ).


% Retrieves specified FSM state attribute.
% Return either the value, if the attribute is found, or 
% { attribute_not_found, AttributeName }.			
getFsmAttribute(FsmState,AttributeName) ->
	case hashtable:lookupEntry( AttributeName, FsmState) of
		
		undefined ->
			{ attribute_not_found, AttributeName } ;
			
		{value,Value} ->
			Value
			
	end.
				
