% Mesh class, for all kinds of graph-based systems (ex: networks).
% A mesh is composed of nodes and links.
% Each node and each link can have an associated content.
% If generateTopologicalView is used, then the content associated to each node
% and link is expected to be the PID of a process that respects the
% class_Digraphable API (see core/generic/src/class_Digraphable.erl).
-module(class_Mesh).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Name,IsCyclic).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).

% Method declarations.
-define(wooper_method_export,addNode/2,addNode/3,addNodes/2,getNodes/1,
	addLink/3,addLink/4,addLink/5,getLinkInformations/2,getLinks/1,
	getNodeFromContent/2,
	findLink/3,findPath/3,findShortestPath/3,getLinksInPath/2,
	setMarkedNodes/2,setMarkedLinks/2,
	generateTopologicalView/2).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Ceylan.Core.Mesh").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% The color used to represent paths:
-define(pathColor,red).


% Implementation notes:
%
% Implementation based on labeled directed graphs, see the digraph module.
%
% Mesh nodes are graph vertices, and mesh links are graph edges.
% Content associated to a mesh node or link corresponds to the dot options 
% (including label) of a graph vertex or graph edge.
% 
% Such a graph is mutable and modified by digraph calls, thus there is no
% need to re-set the graph once modified (no new graph returned, existing one
% updated). 
%
% Undirected graphs could be managed with the same API by duplicating created
% links (both ways).
%
% The content map (content_map) allows to convert a given content associated 
% to a node (say, a PID) to that node (reverse look-up).
	
	
% Constructs a new mesh instance:
%  - Name: the name of the mesh 
%  - IsCyclic: will be cyclic iff true, otherwise will be acyclic
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, Name ),
	
	Option = case IsCyclic of 
	
		true ->
			cyclic;
		
		false ->
			acyclic
	
	end,			
	?send_info([ TraceState, io_lib:format( 
		"Creating a new mesh whose name is ~s, which will be ~w.",
		[ Name, Option ] ) ]),
	
	?setAttributes( TraceState, [ {digraph,digraph:new([Option,private])},
		{graph_label, io_lib:format( "Topological view of mesh '~s'", [Name] )},
		{content_map,hashtable:new()},
		{marked_links,[]}, {marked_nodes,[]},
		{trace_categorization,?TraceEmitterCategorization} ] ).
	
	
% Overriden destructor.
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting mesh." ]),
	digraph:delete( ?getAttr(digraph) ),
	?debug([ "Mesh deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	State.
	
	
	

% Methods section.




% Nodes section.


% Adds specified node to the mesh, with possibly an associated content.
% AddedNode can be a simple node N, or a tuple {N,AssociatedNodeContent}. 
% (oneway).
addNode(State,{AddedNode,AssociatedNodeContent}) ->
	?wooper_return_state_only( 
		add_node( AddedNode, AssociatedNodeContent, State ) );
	
addNode(State,AddedNode) ->
	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), AddedNode ),
	% Content map unchanged.
	?wooper_return_state_only(State).
	

% Adds specified node to the mesh, with possibly an associated content.
% (oneway).
addNode(State,AddedNode,AssociatedNodeContent) ->
	?wooper_return_state_only( 
		add_node(AddedNode, AssociatedNodeContent, State ) ).
		
		
		
% Adds specified list of nodes to the mesh.
% Each added node can be either a simple node N, or a tuple
% {N,AssociatedNodeContent}.
% (oneway).
addNodes(State,NodeList) ->
	?wooper_return_state_only( add_nodes( NodeList, State ) ).
		


% Returns a list of all the nodes of this mesh.
% (request).
getNodes(State) ->
	?wooper_return_state_result( State, digraph:vertices( ?getAttr(digraph) ) ).



% Links section.


% Adds a directed link between FromNode and ToNode.
% Note: 
%  - the nodes must exist in the mesh already, otherwise {bad_mesh_node,V} will
% be returned
%  - if the mesh is acyclic, then {bad_mesh_link,Path} will be returned if a
% cycle would be created
addLink(State,FromNode,ToNode) ->
	case digraph:add_edge( ?getAttr(digraph), FromNode, ToNode ) of

		{error,{bad_edge,Path}} ->
			throw({bad_mesh_link,Path});
		
		{error,{bad_vertex,V}} ->
			throw({bad_mesh_node,V});
		
		_ ->
			?wooper_return_state_only(State)
			
	end.	

	
% Adds a directed link between FromNode and ToNode, and associates 
% AssociatedLinkContent to this link.
% Note: 
%  - the nodes must exist in the mesh already, otherwise {bad_mesh_node,V} will
% be returned
%  - if the mesh is acyclic, then {bad_mesh_link,Path} will be returned if a
% cycle would be created
addLink(State,FromNode,ToNode,AssociatedLinkContent) ->
	case digraph:add_edge( ?getAttr(digraph), FromNode, ToNode, 
			AssociatedLinkContent) of
		
		{error,{bad_edge,Path}} ->
			throw({bad_mesh_link,Path});
		
		{error,{bad_vertex,V}} ->
			throw({bad_mesh_node,V});
		
		_ ->
			?wooper_return_state_only(State)
			
	end.	
	
	
% Adds a directed link between FromNode and ToNode, or updates it, and 
% associates this link to specified content.
% Note: 
%  - the nodes must exist in the mesh already, otherwise {bad_mesh_node,V} will
% be returned
%  - if the mesh is acyclic, then {bad_mesh_link,Path} will be returned if a
% cycle would be created
addLink(State,Link,FromNode,ToNode,AssociatedLinkContent) ->
	case digraph:add_edge( ?getAttr(digraph), Link, FromNode, ToNode,
			AssociatedLinkContent ) of 
		
		{error,{bad_edge,Path}} ->
			throw({bad_mesh_link,Path});
		
		{error,{bad_vertex,V}} ->
			throw({bad_mesh_node,V});
		
		_ ->
			?wooper_return_state_only(State)
			
	end.	


% Returns either {SourceNode,TargetNode,LinkLabel} if specified link is found
% in graph, otherwise link_not_found.
% (const request).
getLinkInformations(State,Link) -> 
	case digraph:edge( ?getAttr(digraph), Link ) of 
	
		false ->
			?wooper_return_state_result( State, link_not_found );
		
		{Link,SourceNode,TargetNode,LinkLabel} ->
			?wooper_return_state_result( State, 
				{SourceNode,TargetNode,LinkLabel} )
				
	end.			
				
	
% Returns a list of all the links of this mesh.
% (const request).
getLinks(State) ->
	?wooper_return_state_result( State, digraph:edges( ?getAttr(digraph) ) ).


% Returns the first node found whose associated is the specified one (if any),
% otherwise returns node_lookup_failed.
% (const request).
getNodeFromContent(State,NodeAssociatedContent) ->
	ContentMap = ?getAttr(content_map),
	case hashtable:lookupEntry(NodeAssociatedContent,ContentMap) of
	
		{value,Node} ->
			?wooper_return_state_result( State, {node,Node} );
			
		undefined ->
			?wooper_return_state_result( State, node_lookup_failed )
				
	end.	


% Searches in mesh for a link from FromNode to ToNode.
% Returns either no_link_found if no link was found, otherwise {Link,LinkLabel}.
findLink(State,FromNode,ToNode) ->
	% Get all edges emanating from FromNode:
	Digraph = ?getAttr(digraph),
	Links = digraph:out_edges( Digraph, FromNode ),
	?wooper_return_state_result( State, 
		find_link_targeting(ToNode,Links,Digraph) ).
	

% Tries to find a path between the source node and the target one.
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
% (const request).
findPath(State,SourceNode,TargetNode) ->
	?wooper_return_state_result( State, digraph:get_path( ?getAttr(digraph),
		SourceNode, TargetNode ) ). 

		
% Tries to find the shortest path between the source node and the target one.
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
% (const request).
findShortestPath(State,SourceNode,TargetNode) ->
	?wooper_return_state_result( State, digraph:get_short_path(
		?getAttr(digraph), SourceNode, TargetNode ) ). 
		

% Returns the list of links corresponding to the specified node path.
% (const request).
getLinksInPath(State,NodeList) ->
	?wooper_return_state_result( State,
		get_links_from( NodeList, ?getAttr(digraph) ) ).
	


% Sets the list of marked nodes.
% These nodes, once the topological view will be generated, will be 
% visually marked. 		
% (oneway).
setMarkedNodes(State,NodeList) ->
	?wooper_return_state_only( ?setAttribute(State,marked_nodes,NodeList) ). 

		
% Sets the list of marked links.
% These links, once the topological view will be generated, will be 
% visually marked. 		
% (oneway).
setMarkedLinks(State,LinkList) ->
	?wooper_return_state_only( ?setAttribute(State,marked_links,LinkList) ). 
		

% Generates a view of current topology of this mesh.
%  - DisplayWanted: boolean telling whether the generated view will be
% displayed to the user (if true)
% (request).	
generateTopologicalView(State,DisplayWanted) ->
	BaseFileName = utils:convert_to_filename( ?getAttr(name) ),
	DigraphFilename = BaseFileName ++ ".graph",
	PNGFilename   = BaseFileName ++ ".png",

	?debug([ io_lib:format( "Generating topology for ~s: "
		"graph in ~s, view in ~s.~n", 
		[ ?getAttr(name), DigraphFilename, PNGFilename ] ) ]),
		
	{ok,DigraphFile} = file:open( DigraphFilename, write ),
	write_graph_header(DigraphFile,State),
	
	write_graph_nodes(DigraphFile,State),
	
	write_graph_links(DigraphFile,State),
	
	write_graph_footer(DigraphFile,State),

	file:close(DigraphFile),
		
	% Dot might issue non-serious warnings:
	%io:format( "dot result: ~w.~n", [DotRes] ),
	
	case os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ DigraphFilename ) of 
		
		[] ->
			ok;
			
		Other ->
			?warning([ io_lib:format( "Dot returned following output: ~s.", 
				[Other] ) ])
	
	end,

	case DisplayWanted of 
	
		true ->	
			utils:display_png_file( PNGFilename );
	
		false ->
			ok
			
	end,	
	?wooper_return_state_result(State,topological_view_generated).

	
		
% Section for helper functions (not methods).


% Adds a node in mesh and register its content in reverse map.
add_node(Node,Content,State) ->
	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), Node, Content ),
	UpdatedContentMap = hashtable:addEntry( Content, Node, 
		?getAttr(content_map) ),
	?setAttribute(State, content_map, UpdatedContentMap ).
	

add_nodes([],State) ->
	State;
	
add_nodes( [{Node,Content}|T], State ) ->
	add_nodes( T, add_node(Node,Content,State) );
	
add_nodes( [Node|T], State ) ->
	digraph:add_vertex(?getAttr(digraph),Node),
	% State "unchanged" (due to side-effect): 
	add_nodes( T, State ).
	



% Writes the graph header for the topology of this mesh in specified file.
write_graph_header(DigraphFile,State) ->
	io:format(DigraphFile, "digraph Mesh_topological_view~n", [] ),
	io:format(DigraphFile, "{~n~n", [] ),
	% size = \"10,10\", fontsize = \"14.0\",
	io:format(DigraphFile, "    graph [ label = \"~s\" ];~n~n",
		[?getAttr(graph_label)] ),
	io:format(DigraphFile, "    node [ style = filled, height = 1, width = 1, "
		"fixedsize = true, shape = circle, fillcolor = lightblue ];~n", [] ).



% Writes the description of graph nodes of this mesh in specified file.
write_graph_nodes(DigraphFile,State) ->
	Nodes = digraph:vertices( ?getAttr(digraph) ),
	write_graph_nodes(DigraphFile,Nodes,State).
	
write_graph_nodes(DigraphFile,[],_State) ->
	io:format(DigraphFile, "\n", []);
	
write_graph_nodes(DigraphFile,[Node|T],State) ->
	{NodeName,NodeOptions} = get_node_graph_informations( 
		?getAttr(digraph), Node ),
	case lists:member(Node,?getAttr(marked_nodes)) of
	
		false ->	
			io:format(DigraphFile, "\"~s\" [~s]~n", [NodeName, 
				format_options(NodeOptions) ]  );

		true ->	
			io:format(DigraphFile, 
				"\"~s\" [~s, style=\"bold,filled\","
				"color=\"~s\",penwidth=\"10\"]~n",
				[ NodeName, format_options(NodeOptions), ?pathColor ]  )
	
	end,	

	write_graph_nodes(DigraphFile,T,State).	
		

% Formats specified options: [ {a,a_value}, {b,b_value}, {c,c_value} ] must
% become: a = "a_value", b = "b_value", c = "c_value".	
format_options(NodeOptions) ->
	utils:join( ", ", lists:map( fun( {Name,Value} ) -> 
		io_lib:format( "~s = \"~s\"", [Name,Value] ) end, NodeOptions ) ).


% Writes the description of graph links of this mesh in specified file.
write_graph_links(DigraphFile,State) ->
	Links = digraph:edges( ?getAttr(digraph) ),
	write_graph_links(DigraphFile,Links,State).
	
	
write_graph_links(DigraphFile,[],_State) ->
	io:format(DigraphFile, "\n", []);
	
write_graph_links(DigraphFile,[Edge|T],State) ->
	{LinkLabel,SourceNodeName,TargetNodeName} = get_link_graph_informations(
		?getAttr(digraph), Edge ),
	case lists:member(Edge,?getAttr(marked_links)) of
	
		false ->	
			io:format(DigraphFile, "\"~s\" -> \"~s\" [label=\"~s\"]~n", 
				[SourceNodeName, TargetNodeName, LinkLabel]  );
	
		true ->	
			io:format(DigraphFile, "\"~s\" -> \"~s\" "
				"[label=\"~s\",color=\"~s\",penwidth=\"5\"]~n", 
				[SourceNodeName, TargetNodeName, LinkLabel, ?pathColor ]  )
	
	end,	
	write_graph_links(DigraphFile,T,State).	



% Writes the graph footer for the topology of this mesh in specified file.
write_graph_footer(DigraphFile,_State) ->
	io:format(DigraphFile, "~n~n}~n~n", []).	


% Returns the graph informations associated to specified graphable node:
% {NodeName,NodeLabel}.
get_node_graph_informations(Digraph,Node) ->
	{Node,NodeContent} = digraph:vertex(Digraph,Node),
	NodeContent ! {getGraphInformations,[],self()},
	receive
	
		{wooper_result,{DigraphableName,OptionList}} ->
			{DigraphableName,OptionList}
			
	end.
	
	
% Returns the graph informations associated to specified link: 
% {LinkLabel,SourceNodeName,TargetNodeName}.
get_link_graph_informations(Digraph,Link) ->
	{Link,SourceNode,TargetNode,LinkLabel} = digraph:edge(Digraph,Link),
	% Just needing here the source and target names:
	{SourceNodeName,_} = get_node_graph_informations(Digraph,SourceNode),
	{TargetNodeName,_} = get_node_graph_informations(Digraph,TargetNode),
	{LinkLabel,SourceNodeName,TargetNodeName}.


% Returns the first link found in link list 'Links' targeting 'TargetNode',
% or, if none is found, no_link_found.
find_link_targeting(_TargetNode,[],_Digraph) ->
	no_link_found;
	
find_link_targeting(TargetNode,[H|T],Digraph) ->
	case digraph:edge(Digraph,H) of 
	
		{H, _SourceNode, TargetNode, LinkLabel} ->
			{H,LinkLabel};
			
		_ ->	
			find_link_targeting(TargetNode,T,Digraph)
			
	end.	
	

get_links_from( NodeList, Digraph ) ->
	get_links_from( NodeList, Digraph, [] ).

	
	
get_links_from( [N1,N2|T], Digraph, Acc ) ->
	OutLinks = digraph:out_edges( Digraph, N1 ),
	{Link,_Label} = find_link_targeting( N2,OutLinks,Digraph),
	get_links_from( [N2|T], Digraph, [ Link | Acc ] );

get_links_from( _LastNode, _Digraph, Acc ) ->
	lists:reverse( Acc ).
