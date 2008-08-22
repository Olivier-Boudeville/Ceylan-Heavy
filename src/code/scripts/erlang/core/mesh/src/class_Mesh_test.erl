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


	MyMesh ! {getNodeFromContent,[ "hello" ],self()},
	receive
	
		{wooper_result,{node,SecondNode}} ->
			?test_info([ "Node look-up from content succeeded." ])
	
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
			{label,"My second graphable"}, {bgcolor,yellow}, 
			{fillcolor,green}, {color,red}, {pencolor,cyan} ] ) },
		{ third_graphable,  class_Graphable:new( [ 
			{label,"My third graphable"}, {shape,hexagon}, {color,blue} ] ) },
		{ fourth_graphable,  class_Graphable:new( "My fourth graphable" ) }
	]] },
	
	MyGraphableMesh ! {addLink,[first_graphable,second_graphable,
		"I am a link from first to second"]},

	MyGraphableMesh ! {addLink,[first_graphable,third_graphable,
		"I am a link from first to third"]},

	MyGraphableMesh ! {addLink,[second_graphable,third_graphable,
		"I am a link from second to third"]},
	
	MyGraphableMesh ! {addLink,[second_graphable,fourth_graphable,
		"I am a link from second to fourth"]},


	MyGraphableMesh ! {findLink,[first_graphable,second_graphable],self()},
	receive
	
		{wooper_result, {Link,LinkLabel} } ->
			?test_info([ io_lib:format( 
				"Link from first to second graphable is ~w, whose label is ~s.",
					[ Link, LinkLabel ] ) ])
	
	end,
	

	MyGraphableMesh ! {findPath,[first_graphable,fourth_graphable],self()},
	receive
	
		{wooper_result,Path} ->
			?test_info([ io_lib:format( 
				"Path from first to fourth graphable: ~w.", [ Path ] ) ])
	
	end,


	MyGraphableMesh ! {findShortestPath,
		[first_graphable,fourth_graphable],self()},
	receive
	
		{wooper_result,ShortestPath} ->
			?test_info([ io_lib:format( 
				"Shortest path from first to fourth graphable: ~w.", 
			[ ShortestPath ] ) ])
	
	end,

	MyGraphableMesh ! {setMarkedNodes,[Path]},
		
		
	MyGraphableMesh ! {getLinksInPath,[ShortestPath],self()},
	receive
	
		{wooper_result,PathLinks} ->
			?test_info([ io_lib:format( 
				"Links from first to fourth graphable: ~w.", [ PathLinks ] ) ])
	
	end,

	MyGraphableMesh ! {setMarkedLinks,[PathLinks]},
	
	?generateTopologicalViewFor(MyGraphableMesh),

	MyGraphableMesh ! delete,
		

	?test_stop.

