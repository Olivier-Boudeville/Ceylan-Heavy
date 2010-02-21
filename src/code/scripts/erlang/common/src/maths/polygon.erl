% Copyright (C) 2003-2010 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for polygon management.
% Coordinates are expected to be often integers, when used for rendering.
% See polygon_test.erl for the corresponding test.
-module(polygon).


-include("polygon.hrl").



% Construction-related section.
-export([ get_triangle/3, get_upright_square/2 ]).


% Operations on polygons.
-export([ get_diameter/1, get_smallest_enclosing_rectangle/1, render/2, 
		  to_string/1 ]).


% Color-related section.
-export([ set_edge_color/2, get_edge_color/1, 
		  set_fill_color/2, get_fill_color/1 ]). 


% Bounding-box related section.
-export([ update_bounding_box/2 ]). 




% Construction-related section.


% Returns a triangle corresponding to the specified three vertices.
get_triangle( V1, V2, V3 ) ->
	#polygon{ vertices = [V1,V2,V3] }.


% Returns an upright square corresponding to the specified center and edge 
% length.
get_upright_square( _Center = {Xc,Yc}, EdgeLength ) ->
	Offset = erlang:round(EdgeLength / 2),
	X1 = Xc - Offset,
	X2 = Xc + Offset,
	Y1 = Yc - Offset,
	Y2 = Yc + Offset,
	#polygon{ vertices = [ {X1,Y1}, {X2,Y1}, {X2,Y2}, {X1,Y2} ] }.




% Operations on polygons.


% Returns a polygon diameter, i.e. two points in the polygon which are at the
% maximum distance one of the other.
% Returns {V1,V2,D} when V1 and V2 are the endpoints
% of a diameter and D is its square length: D = square_distance(V1,V2). 
get_diameter( Polygon ) ->

	case Polygon#polygon.vertices of

		[] ->
			throw( no_vertex );

		[_Vertex] ->
			throw( single_vertex );

		ListWithAtLeastTwoVertices ->
			% There are at least two vertices:
			linear_2D:compute_max_overall_distance( ListWithAtLeastTwoVertices )
	
	end.



% Returns the smallest upright rectangle which encompasses the specified 
% polygon.
% More precisely, {TopLeftCorner,BottomRightCorner} is returned, which 
% defines the rectangle from two opposite points.
get_smallest_enclosing_rectangle( Polygon ) ->
	
	case Polygon#polygon.vertices of

		[] ->
			throw( no_vertex );

		[_Vertex] ->
			throw( single_vertex );

		ListWithAtLeastTwoVertices ->
			linear_2D:compute_smallest_enclosing_rectangle( 
			  ListWithAtLeastTwoVertices )

	end.





% Color-related section.


% Sets the edge color of specified polygon.
set_edge_color( Color, Polygon ) ->
	Polygon#polygon{ rendering = option_list:set( {fg,gui:get_color(Color)},
										Polygon#polygon.rendering ) }.

% Returns the current edge color of the specified polygon, if specified, 
% otherwise 'undefined'.	  
get_edge_color( Polygon ) ->
	option_list:lookup( fg, Polygon#polygon.rendering ).



% Sets the fill color of specified polygon.
% Use 'none' to disable filling.
set_fill_color( Color, Polygon ) ->
	Polygon#polygon{ rendering = option_list:set( {fill,gui:get_color(Color)},
										Polygon#polygon.rendering ) }.

% Returns the current fill color of the specified polygon, if specified, 
% otherwise 'undefined'.	  		  
get_fill_color( Polygon ) ->
	option_list:lookup( fill, Polygon#polygon.rendering ).



% Returns options for the rendering of this polygon that can be directly
% passed to the graphical back-end.
get_rendering_options( Polygon ) ->
	Polygon#polygon.rendering.


% Renders specified polygon in specified canvas.
% Throws an exception if the polygon is not valid.
render( Polygon, Canvas ) ->

	%io:format( "Rendering polygon:~n~s.~n", [to_string(Polygon)] ),

	case Polygon#polygon.vertices of

		[] ->
			throw( null_polygon );

		[_Vertex] ->
			throw( one_vertex_polygon );

		Vertices ->

			Options = [ {coords,Vertices} | get_rendering_options(Polygon) ],
			gs:create( polygon, Canvas, Options ),
			
			case Polygon#polygon.bounding_box of

				{circle,Center,SquareRadius} ->
					gui:draw_circle( Center, math:sqrt(SquareRadius), Canvas ),
					gui:draw_cross( Center, _EdgeLength = 4, Canvas );

				undefined ->
					ok

			end

	end.




% Returns a textual description of the specified polygon.
to_string( Polygon ) ->
	
	BBText = case Polygon#polygon.bounding_box of
		
				 undefined ->
					 "none available";

				 BB ->
					 bounding_box:to_string(BB)
						 
	end,

 	io_lib:format(  "  + vertices: ~w~n", [Polygon#polygon.vertices] )
 		++ io_lib:format( "  + edge color: ~w~n", [get_edge_color(Polygon)] )
 		++ io_lib:format( "  + fill color: ~w~n", [get_fill_color(Polygon)] )
 		++ io_lib:format( "  + bounding-box: ~s~n", [BBText] ).



% Bounding-box related section.


% Updates, for the specified polygon, its internal bounding-box, with regard 
% to the specified bounding-box request. 
% Returns a polygon with updated information.
% The lazy circle bounding box is fast to determine, but not optimal:
update_bounding_box( lazy_circle, Polygon ) ->
	{Center,SquareRadius} = bounding_box:get_lazy_circle_box( 
							  Polygon#polygon.vertices ),
	Polygon#polygon{ bounding_box = {circle,Center,SquareRadius} }.



