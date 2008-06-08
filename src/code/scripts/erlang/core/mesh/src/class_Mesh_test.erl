% Unit tests for the Mesh class implementation.
% See the class_Mesh module.
-module(class_Mesh_test).


-define(Tested_modules,[class_Mesh]).


% For all facilities common to all tests:
-include("test_constructs.hrl").




% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Mesh." ]),
			
	% Acyclic:		
	MyMesh = class_Mesh:new_link( "My test mesh", false ),
	
	
	?test_info([ "Testing node management." ]),
	
	FirstNode  = first,
	SecondNode = second,
	ThirdNode  = third,

	% FirstNode  -> SecondNode
	% FirstNode  -> ThirdNode
	% SecondNode -> ThirdNode
	
	MyMesh ! {addNode,FirstNode},
	
	NodeList = [ {SecondNode,"hello"}, ThirdNode ],
	
	% Note the list in list:
	MyMesh ! {addNodes,[NodeList]},

	MyMesh ! {getNodes,[],self()},
	
	receive
	
		{wooper_result,Nodes} ->
			?test_info([ io_lib:format( 
				"This mesh has following nodes defined: ~w.",
				[Nodes] ) ])
	
	end,




	?test_info([ "Testing link management." ]),

	MyMesh ! {getLinks,[],self()},
	
	receive
	
		{wooper_result,Links} ->
			?test_info([ io_lib:format( 
				"This mesh has following links defined: ~w.",
				[Links] ) ])
	
	end,
	
	MyMesh ! {addLink,[FirstNode,second]},
	MyMesh ! {addLink,[FirstNode,ThirdNode,"I am a link content"]},
	MyMesh ! {addLink,[second,ThirdNode]},
	
	MyMesh ! {getLinks,[],self()},
	
	receive
	
		{wooper_result,NewLinks} ->
			?test_info([ io_lib:format( "This mesh has now following links "
				" defined (not really readable): ~w.", [NewLinks] ) ])
	
	end,

	% Uncomment this to see what happens when a cycle is made in an acyclic 
	% mesh:
	%MyMesh ! {addLink,[ThirdNode,FirstNode]},

	MyMesh ! delete,


	?test_info([ "Testing mesh rendering as a graph, "
		"using now Graphable instances as content." ]),

	% Acyclic:		
	MyGraphableMesh = class_Mesh:new_link( "My test graphable mesh", false ),
		
	MyGraphableMesh ! { addNodes, [[
		{ first_graphable,  class_Graphable:new( "My first graphable" ) },
		{ second_graphable, class_Graphable:new( [ 
			{label,"My second graphable"}, {color,red} ] ) },
		{ third_graphable,  class_Graphable:new( [ 
			{label,"My third graphable"}, {shape,hexagon}, {color,blue} ] ) }
	]] },
	
	MyGraphableMesh ! {addLink,[first_graphable,second_graphable,
		"I am a link from first to second"]},

	MyGraphableMesh ! {addLink,[first_graphable,third_graphable,
		"I am a link from first to third"]},

	MyGraphableMesh ! {addLink,[second_graphable,third_graphable,
		"I am a link from second to third"]},


	MyGraphableMesh ! {generateTopologicalView,[],self()},
	
	receive
	
		{wooper_result,topological_view_generated} ->
			?test_info([ "Topological view generated." ])
	
	end,

	MyGraphableMesh ! delete,
		

	?test_stop.

