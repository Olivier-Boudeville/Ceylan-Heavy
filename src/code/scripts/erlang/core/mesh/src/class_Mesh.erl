% Mesh class, for all kinds of graph-based systems (ex: networks).
% A mesh is composed of nodes and links.
% Each node and each link can have an associated content.
% If generateTopologicalView is used, then the content associated to each node
% and link is expected to be the PID of a process that respects the
% class_Graphable API. 
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
	addLink/3,addLink/4,addLink/5,getLinks/1,
	generateTopologicalView/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Ceylan.Core.Mesh").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% Implementation Notes:
%
% Based on labeled directed graphs, see the digraph module.
%
% Mesh nodes are graph vertices, and mesh links are graph edges.
% Content associated to a mesh node or link corresponds to the label of a graph 
% vertex or graph edge.
% 
% Such a graph is mutable and modified by digraph calls, thus there is no
% need to re-set the graph once modified (no new graph returned, existing one
% updated). 
%
% Undirected graphs could be managed with the same API by duplicating created
% links (both ways).
	
	
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
	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), AddedNode, AssociatedNodeContent ),
	?wooper_return_state_only(State);
	
addNode(State,AddedNode) ->
	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), AddedNode ),
	?wooper_return_state_only(State).
	

% Adds specified node to the mesh, with possibly an associated content.
% AddedNode can be a simple node N, or a tuple {N,AssociatedNodeContent}. 
% (oneway).
addNode(State,AddedNode,AssociatedNodeContent) ->
	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), AddedNode, AssociatedNodeContent ),
	?wooper_return_state_only(State).
	
		
% Adds specified list of nodes to the mesh.
% Each added node can be either a simple node N, or a tuple
% {N,AssociatedNodeContent}.
% (oneway).
addNodes(State,NodeList) ->
	add_node( ?getAttr(digraph), NodeList ),
	?wooper_return_state_only(State).
		

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

	
% Returns a list of all the links of this mesh.
% (request).
getLinks(State) ->
	?wooper_return_state_result( State, digraph:edges( ?getAttr(digraph) ) ).
	
		
% Generates a view of current topology of this mesh.
% (request).	
generateTopologicalView(State) ->
	BaseFileName = utils:convert_to_filename( ?getAttr(name) ),
	GraphFilename = BaseFileName ++ ".graph",
	PNGFilename   = BaseFileName ++ ".png",

	?debug([ io_lib:format( "Generating topology for ~s: "
		"graph in ~s, view in ~s.~n", 
		[ ?getAttr(name), GraphFilename, PNGFilename ] ) ]),
		
	{ok,GraphFile} = file:open( GraphFilename, write ),
	write_graph_header(GraphFile,State),
	
	write_graph_nodes(GraphFile,State),
	
	write_graph_links(GraphFile,State),
	
	write_graph_footer(GraphFile,State),

	file:close(GraphFile),
		
	% Dot might issue non-serious warnings:
	%io:format( "dot result: ~w.~n", [DotRes] ),
	
	case os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename ) of 
		
		[] ->
			ok;
			
		Other ->
			?warning([ io_lib:format( "Dot returned following output: ~s.", 
				[Other] ) ])
	
	end,
	
	% Viewer is 'eye of gnome' here: 
	os:cmd( "eog " ++ PNGFilename ++ " &" ),

	?wooper_return_state_result(State,topological_view_generated).

	
		
% Section for helper functions (not methods).


	
add_node(_Digraph,[]) ->
	ok;
	
add_node(Digraph,[{Node,Content}|T]) ->
	digraph:add_vertex(Digraph,Node,Content),
	add_node(Digraph,T);
	
add_node(Digraph,[Node|T]) ->
	digraph:add_vertex(Digraph,Node),
	add_node(Digraph,T).
	


% Writes the graph header for the topology of this mesh in specified file.
write_graph_header(GraphFile,State) ->
	io:format(GraphFile, "digraph Mesh_topological_view~n", [] ),
	io:format(GraphFile, "{~n~n", [] ),
	% size = \"10,10\", fontsize = \"14.0\",
	io:format(GraphFile, "    graph [ label = \"~s\" ];~n~n",
		[?getAttr(graph_label)] ),
	io:format(GraphFile, "    node [ style = filled, height = 1, width = 1, fixedsize = true, shape = circle, color = lightblue ];~n", [] ).



% Writes the description of graph nodes of this mesh in specified file.
write_graph_nodes(GraphFile,State) ->
	Nodes = digraph:vertices( ?getAttr(digraph) ),
	write_graph_nodes(GraphFile,Nodes,State).
	
write_graph_nodes(GraphFile,[],_State) ->
	io:format(GraphFile, "\n", []);
	
write_graph_nodes(GraphFile,[H|T],State) ->
	{NodeName,NodeLabel} = get_node_graph_informations( ?getAttr(digraph), H ),
	io:format(GraphFile, "\"~s\" [label=\"~s\"]~n", [NodeName, NodeLabel]  ),
	write_graph_nodes(GraphFile,T,State).	
		
	
% Writes the description of graph links of this mesh in specified file.
write_graph_links(GraphFile,State) ->
	Links = digraph:edges( ?getAttr(digraph) ),
	write_graph_links(GraphFile,Links,State).
	
	
write_graph_links(GraphFile,[],_State) ->
	io:format(GraphFile, "\n", []);
	
write_graph_links(GraphFile,[Edge|T],State) ->
	{LinkLabel,SourceNodeName,TargetNodeName} = get_link_graph_informations(
		?getAttr(digraph), Edge ),
	io:format(GraphFile, "\"~s\" -> \"~s\" [label=\"~s\"]~n", 
		[SourceNodeName, TargetNodeName, LinkLabel]  ),
	write_graph_links(GraphFile,T,State).	



% Writes the graph footer for the topology of this mesh in specified file.
write_graph_footer(GraphFile,_State) ->
	io:format(GraphFile, "~n~n}~n~n", []).	


% Returns the graph informations associated to specified node:
% {NodeName,NodeLabel}.
get_node_graph_informations(Digraph,Node) ->
	{Node,NodeContent} = digraph:vertex(Digraph,Node),
	NodeContent ! {getGraphInformations,[],self()},
	receive
	
		{wooper_result,NodeInfos} ->
			NodeInfos
			
	end.
	
	
% Returns the graph informations associated to specified link: 
% {LinkLabel,SourceNodeName,TargetNodeName}.
get_link_graph_informations(Digraph,Link) ->
	{Link,SourceNode,TargetNode,LinkLabel} = digraph:edge(Digraph,Link),
	{SourceNodeName,_} = get_node_graph_informations(Digraph,SourceNode),
	{TargetNodeName,_} = get_node_graph_informations(Digraph,TargetNode),
	{LinkLabel,SourceNodeName,TargetNodeName}.

