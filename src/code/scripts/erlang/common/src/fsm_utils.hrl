
% Sets specified attribute of the instance to the specified value, thanks to
% specified FSM state.
% Returns an updated state.
-define(setFsmAttribute(FsmState,AttributeName,AttributeValue),
	hashtable:addEntry( AttributeName, AttributeValue, FsmState )
).
	
	

% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName }'.
% See also: getAttr/1
-define(getFsmAttribute(FsmState,AttributeName),
	case hashtable:lookupEntry( AttributeName, FsmState) of
		
		undefined ->
			{ attribute_not_found, AttributeName } ;
			
		{value,Value} ->
			Value
			
	end			
).


% Returns the value associated to specified named-designated attribute, if 
% found, otherwise returns '{ attribute_not_found, AttributeName }'.
% Beware to the implicit use of the 'FsmState' variable: in some cases other
% states should be used. See the getAttribute/2 macro.
-define(getFsmAttr(AttributeName),?getFsmAttribute(FsmState,AttributeName)).

