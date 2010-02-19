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


% Gathering of various two dimensional linear facilities.
% See linear_2D_test.erl for the corresponding test.
-module(linear_2D).




% Operations on points:
-export([ square_distance/2, distance/2, 
		  cross_product/2, roundify/1, get_integer_center/2, get_center/2 ]).

	
% Operations on vectors:
-export([ vectorize/2, square_magnitude( _V={X,Y} ) ->
	X*X+Y*Y.


% Returns the magnitude of the specified vector.
magnitude( V ) ->
	math:sqrt( square_magnitude(V) ).


% Scales specified vector of specified factor.
scale( _V={X,Y}, Factor ) ->
	{Factor*X,Factor*Y}.


% Returns the specified vector with an unit length (magnitude of 1):
% (epsilon-based test for null vectors with floating-point coordinates could
% be done here).
make_unit( {0,0} ) ->
	throw( cannot_make_null_vector_unit );

make_uni ]).
	
	

% Point section.


% Returns the square of the distance between the two specified points.
% For comparison purposes, computing the square root is useless.
% Could rely on vectorize and square_magnitude as well.
square_distance( {X1,Y1}, {X2,Y2} ) ->
	XDiff = X2-X1,
	YDiff = Y2-Y1,
	XDiff*XDiff + YDiff*YDiff.


% Returns the distance between the two specified points.
% For comparison purposes, computing the square root is useless.
% Could rely on vectorize and magnitude as well.
distance( P1, P2 ) ->
	math:sqrt( square_distance(P1,P2) ).



% Returns the cross-product of the two specified 2D points, i.e. the magnitude
% of the vector that would result from a regular 3D cross product of the input
% vectors, taking their Z values implicitly as 0.
cross_product( {X1,Y1}, {X2,Y2} ) ->
	X1*Y2 - Y1*X1.


% Returns a point (or vector) whose coordinates have been rounded to nearest
% integer.
roundify( {X,Y} ) ->
	{ erlang:round(X), erlang:round(Y) }.


% Returns a vertex corresponding the middle of the two specified vertices,
% returned with integer coordinates.
get_integer_center( P1, P2 ) ->
	roundify( get_center(P1,P2) ).


% Returns a vertex corresponding the middle of the two specified vertices,
% returned with possibly floating-point coordinates.
get_center( {X1,Y1}, {X2,Y2} ) ->
	{ (X1+X2)/2 ), (Y1+Y2)/2 }.




% Vector section.


% Returns a vector V made from the specified two points: V=P2-P1.
vectorize( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	{X2-X1,Y2-Y1}.


% Returns the square of the magnitude of the specified vector.
square_magnitude( _V={X,Y} ) ->
	X*X+Y*Y.


% Returns the magnitude of the specified vector.
magnitude( V ) ->
	math:sqrt( square_magnitude(V) ).


% Scales specified vector of specified factor.
scale( _V={X,Y}, Factor ) ->
	{Factor*X,Factor*Y}.


% Returns the specified vector with an unit length (magnitude of 1):
% (epsilon-based test for null vectors with floating-point coordinates could
% be done here).
make_unit( {0,0} ) ->
	throw( cannot_make_null_vector_unit );

make_unit( V ) ->
	scale( V, 1/magnitude(V) ).


