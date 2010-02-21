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


% Gathering of various facilities for bounding box management.
% Currently supported bounding box are:
%  - circle, either obtained from the 'lazy' algorithm or the 'mec' algorithm
%
% With the lazy algorithm, circle parameters are simply deduced from the 
% smallest enclosing rectangle; it is fast and easy, yet less precis than MEC.
%
% MEC leads to determine the Minimal Enclosing Circle. The operation involves
% computing the convex hull of the points. It is expensive, but not a problem
% when precomputing.
%
% See bounding_box_test.erl for the corresponding test.
-module(bounding_box).



-export([ get_lazy_circle_box/1, to_string/1 ]). 


% Returns a disc which is a bounding-box for the specified list of points, which
% must not be empty.
% Returns the disc information: {Center,SquareRadius}.
get_lazy_circle_box( PointList ) ->
	{TopLeft,BottomRight} = linear_2D:compute_smallest_enclosing_rectangle( 
							  PointList ),
	Center = linear_2D:get_integer_center( TopLeft, BottomRight ),
	% We divide by 4 as we are dealing with squared quantities:
	SquareRadius = linear_2D:square_distance( TopLeft, BottomRight ) / 4,
	{Center,SquareRadius}.


to_string( {circle,Center,SquareRadius} ) -> 
	io_lib:format( "circle whose center is ~w and square radius is ~w",
				   [Center,SquareRadius] ).


