% Unit tests for the Graphable class implementation.
% See the class_Graphable module.
-module(class_Graphable_test).


-define(Tested_modules,[class_Graphable]).


% For all facilities common to all tests:
-include("test_constructs.hrl").




% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Graphable." ]),
			
	MyGraphable = class_Graphable:new_link( [ {label,"hello"}, {color,red} ] ),
	
	
	MyGraphable ! {getNodeName,[],self()},
	receive
	
		{wooper_result,NodeName} ->
			?test_info([ io_lib:format( "Node name: ~s.", [NodeName] ) ])
			
	end,

	MyGraphable ! {getLabel,[],self()},
	receive
	
		{wooper_result,Label} ->
			?test_info([ io_lib:format( "Label: ~s.", [Label] ) ])
			
	end,

	
	MyGraphable ! {getGraphInformations,[],self()},
	receive
	
		{wooper_result,{_OtherNodeName,Infos}} ->
			?test_info([ io_lib:format( "Graph informations: ~w.", [Infos] ) ])
			
	end,
		
	MyGraphable ! delete,

	?test_stop.

