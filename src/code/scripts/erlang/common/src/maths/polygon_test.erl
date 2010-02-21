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


% Unit tests for polygon management.
% Depends on the gui module.
% See the polygon tested module.
-module(polygon_test).


-export([ run/0 ]).

-define(Tested_module,polygon).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	MyTriangle = polygon:update_bounding_box( lazy_circle, 
	  polygon:set_edge_color( yellow, 
			  polygon:get_triangle( {110,110}, {250,155}, {120,335} ) ) ),
	
	io:format( "   Triangle description:~n~s~n", 
			   [polygon:to_string(MyTriangle)] ),


	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
	  polygon:set_fill_color( red, 
			  polygon:get_upright_square( _Center = {250,250}, 
										  _EdgeLength = 50 ) ) ),
  
	io:format( "   Upright square description:~n~s~n", 
			   [polygon:to_string(MyUprightSquare)] ),


	io:format( "   Diameter information ({P1,P2,SquareDistance}):~n"
			   "  - for the triangle, we have: ~w~n"
			   "  - for the upright square, we have: ~w~n~n", 
			   [polygon:get_diameter(MyTriangle),
				polygon:get_diameter(MyUprightSquare)] ),

	io:format( "   Smallest enclosing rectangle "
			   "({TopLeftCorner,BottomRightCorner}):~n"
			   "  - for the triangle, we have: ~w~n"
			   "  - for the upright square, we have: ~w~n", 
			   [polygon:get_smallest_enclosing_rectangle(MyTriangle),
				polygon:get_smallest_enclosing_rectangle(MyUprightSquare)] ),

	
	% Rendering tests: see gui_test.erl.
	  
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

