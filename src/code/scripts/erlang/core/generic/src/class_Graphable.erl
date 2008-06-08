% Graphable class, base of all instances able to output a textual description
% of their state in the context of graph rendering.
% See also: core/mesh/src/class_Mesh.erl
-module(class_Graphable).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,OptionParameter).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).

% Method declarations.
-define(wooper_method_export,getNodeName/1,getLabel/1,setLabel/2,
	getGraphInformations/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Implementation Note:
%
% Being a trace emitter is not strictly needed, as it leads to useless 
% diamond-shaped multiple inheritance.

	
% Constructs a new graphable instance. 
% OptionParameter is:
%  - either a label, like "hello"
%  - or a list of option pairs like {dot_option_name,option_value} in which
% at least the label is defined. 
% Ex: [ {label,"hello"}, {color,"red"} ].
construct(State,?wooper_construct_parameters) ->
	% First the direct mother classes, then this class-specific actions:
	interpret_option_list(State,OptionParameter).
	
	
% Overriden destructor.
delete(State) ->
	% Class-specific actions:
	% Then call the direct mother class counterparts and allow chaining:
	State.
	
	
	

% Methods section.


% Returns a name, derived from current PID, adequate to be a node identifier.
% (const request)
getNodeName(State) ->
	?wooper_return_state_result( State, forge_node_name() ).



% Returns the description of this Graphable.
% (const request)
getLabel(State) ->
	?wooper_return_state_result( State, ?getAttr(label) ).

% Sets the label of this Graphable.
% (oneway)
setLabel(State,NewLabel) ->
	?wooper_return_state_only( ?setAttribute( State, label,	NewLabel ) ).
	

% Returns {GraphableName,OptionList} where GraphableName is the generated name
% for this graphable, and OptionList is the list of all attribute name/value
% pairs, which are supposed to be dot options.
% (const request)
getGraphInformations(State) ->
	?wooper_return_state_result( State, {forge_node_name(),
		wooper_get_all_attributes(State)} ).


		
% Section for helper functions (not methods).

% Interprets the option list specified for a graphable.


% Sets the relevant options in state.
interpret_option_list( State, [] ) ->
	State;
	
interpret_option_list( State, [{label,Label}|T] ) ->
	interpret_option_list( 
		?setAttribute( State, label, transform_label(Label) ), T );

% Beware to attribute name clashing:
interpret_option_list( State, [{OptionName,OptionValue}|T] ) ->
	interpret_option_list( 
		?setAttribute( State, OptionName, OptionValue ), T );

interpret_option_list( State, Label ) ->
	interpret_option_list( 
		?setAttribute( State, label, transform_label(Label) ), [] ).
	

forge_node_name() ->
	% Ex: "<0.59.0>":
	PidAsString = hd( io_lib:format( "~w", [self()] ) ),
	% Ex: "0.59.0":	
	string:substr( PidAsString, 2, length(PidAsString)-2 ).


% Splits specified label, one word per line.	
transform_label(Label) ->
	separate_in_lines( string:tokens(Label," ") ).

separate_in_lines( WordList ) ->
	separate_in_lines( WordList, "" ).
	
		
separate_in_lines( [], ResultingString ) ->
	ResultingString;
	
separate_in_lines( [H|T], ResultingString ) ->
	separate_in_lines( T, ResultingString ++ H ++ "\\n" ).
	

