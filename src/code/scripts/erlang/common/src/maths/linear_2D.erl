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

% Operations on set of points:
-export([ compute_smallest_enclosing_rectangle/1,
		  compute_max_overall_distance/1, compute_convex_hull/1 ]).
	
% Operations on vectors:
-export([ vectorize/2, square_magnitude/1, magnitude/1, scale/2, make_unit/1 ]).


% Temp:
-export([ find_pivot/1, sort_by_angle/2 ]).
	

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
	X1*Y2 - Y1*X2.


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
	{ (X1+X2)/2, (Y1+Y2)/2 }.




% Section for sets of points.


% Computes the smallest rectangle that encloses the specified list of points.
% Returns { TopLeft, BottomRight }.
compute_smallest_enclosing_rectangle( Points ) ->
	compute_smallest_enclosing_rectangle( Points, undefined, undefined ).


compute_smallest_enclosing_rectangle( [], TopLeft, BottomRight ) ->
	{ TopLeft, BottomRight };

compute_smallest_enclosing_rectangle( [P|Others], undefined, undefined ) ->
	% First found initializes best, knowing at least two points are expected:
	compute_smallest_enclosing_rectangle( Others, _TopLeft=P, _BottomRight=P );

compute_smallest_enclosing_rectangle( [{X,Y}|Others], {Xt,Yt}, {Xb,Yb} ) ->
	Xmin = erlang:min( X, Xt ),
	Ymin = erlang:min( Y, Yt ),
	Xmax = erlang:max( X, Xb ),
	Ymax = erlang:max( Y, Yb ),											
	compute_smallest_enclosing_rectangle( Others, {Xmin,Ymin}, {Xmax,Ymax} ).


% Computes the maximum distance between two points in the specified list
% of points.
% Returns {P1,P2,square_distance(P1,P2)} so that (square) distance is maximal.
% We ensure that each internal edge is examined only once: when the distance
% between a given vertex V and all other vertices have been computed, V is
% removed from the list and a new mAximum is searched within this subset. 
% Here there is only one vertex left:
compute_max_overall_distance( Points ) when length(Points) < 2 ->
	throw( {no_computable_overall_distance,Points} );

compute_max_overall_distance( Points ) ->
	compute_max_overall_distance( Points, undefined ).


compute_max_overall_distance( [_H], Longest ) ->
	Longest;

% Here we have not compute a distance yet:
compute_max_overall_distance( [H|Others], undefined ) ->
	FirstEntry = compute_max_distance_between( H, Others ),
	compute_max_overall_distance( Others, _FirstBest=FirstEntry );
 
% At least one other vertex remains, and at least one distance was computed:
compute_max_overall_distance( [H|Others], 
							  Best = {_V1,_V2,LongestSquareDistance} ) ->

	case compute_max_distance_between( H, Others, undefined ) of

		{PmaxForH,LongestSquareDistanceFromH} when LongestSquareDistanceFromH >
												   LongestSquareDistance ->
			% We have a new winner:
			compute_max_overall_distance( Others, 
			   {H,PmaxForH,LongestSquareDistanceFromH} );
										   
		_Other ->							 
			% Here LongestSquareDistance is not beaten:
			compute_max_overall_distance( Others, Best )
				
	end.



% Computes the maximum distance between a point (P) and a list of other
% points.  
% Returns {P,Pmax,LongestSquareDistance} with LongestSquareDistance
% being the distance between P and Pmax, Pmax being chosen so that
% LongestSquareDistance is maximal.  
% As there must have been at least one point in the list, Pmax exists here
% (never undefined):
compute_max_distance_between( _P, [] ) ->
	throw( no_computable_max_distance );
	
compute_max_distance_between( P, Points ) ->
	compute_max_distance_between( P, Points, undefined ).


compute_max_distance_between( P, [], {Pmax,LongestSquareDistance} ) ->
	{P,Pmax,LongestSquareDistance};

compute_max_distance_between( P, [Pnew|OtherPoints], undefined ) ->
	% First point examined is at first by construction the first best:
	compute_max_distance_between( P, OtherPoints, 
								  {Pnew,linear_2D:square_distance(P,Pnew)} );

compute_max_distance_between( P, [Pnew|OtherPoints],
							  Best = {_Pmax,LongestSquareDistance} ) ->

	case linear_2D:square_distance(P,Pnew) of

		SquareDistance when SquareDistance > LongestSquareDistance ->
			% We have a new winner:
			compute_max_distance_between( P, OtherPoints, 
										  {Pnew,SquareDistance} );

		_LesserSquareDistance ->
			% Previous best not beaten, let's keep it:
			compute_max_distance_between( P, OtherPoints, Best )
	end.




% Sorting by angle section.


% Finds the pivot, i.e. the leftmost point with the highest ordinate.
% The point list is supposed not having duplicates.
% Returns {Pivot,PivotLessList} where PivotLessList is the (unordered)
% input list, without the Pivot.  
find_pivot( _PointList = [FirstPivot|Others] ) ->
	% First found is the first pivot:
	find_pivot( Others, FirstPivot, _NewList=[] ).


find_pivot( [], Pivot, NewList ) ->
	{Pivot,NewList};

% Higher than the pivot, thus not wanted as pivot:
find_pivot( [Point={_X,Y}|Others], Pivot={_Xp,Yp}, NewList ) when Y<Yp ->
	find_pivot( Others, Pivot, [Point|NewList] );

% Lower than the pivot, thus wanted:
find_pivot( [Point={_X,Y}|Others], PreviousPivot={_Xp,Yp}, NewList ) when Y>Yp ->
	find_pivot( Others, Point, [PreviousPivot|NewList] );


% Same level as the pivot, but at its right, thus not wanted:
find_pivot( [Point={X,_Y}|Others], Pivot={Xp,_Y}, NewList ) when X>Xp ->
	find_pivot( Others, Pivot, [Point|NewList] );

% Same level as the pivot, but at its left, thus wanted:
find_pivot( [Point={X,_Yp}|Others], PreviousPivot={Xp,_Yp}, NewList ) when X<Xp ->	
	find_pivot( Others, Point, [PreviousPivot|NewList] );

% Duplicated pivot, abnormal:
find_pivot( [Pivot|_Others], Pivot, _NewList ) ->
	throw( {duplicate_pivot,Pivot} ).



% Returns a list containing the points sorted according to an increasing angle
% between the abscissa axis and the vector from the pivot that each point.
% Note: all points having the same abscissa as the pivot, except the highest one,
% will be removed from the returned list.
sort_by_angle( Pivot, Points ) ->
	sort_by_angle( Pivot, Points, _LeftPoints=[], _MiddlePoint=undefined, 
				   _RightPoints=[] ).


% LeftPoints and RightPoints are lists of {Angle,Point} pairs.
sort_by_angle( _Pivot, _Points=[], LeftPoints, undefined, RightPoints ) ->
	%io:format( "sort_by_angle: no middle point found.~n" ),
	% Not having a middle point to integrate here:
	L = lists:keysort( _Index=1, LeftPoints )
		++ lists:keysort( _Index=1, RightPoints ),
	%io:format( "Full list: ~w.~n", [L] ),
	reverse_and_drop_angle(L,[]);

sort_by_angle( _Pivot, _Points=[], LeftPoints, MiddlePoint, RightPoints ) ->
	%io:format( "sort_by_angle: at least one middle point found.~n" ),
	L = lists:keysort( _Index=1, LeftPoints ) 
		++ [{dummy,MiddlePoint}|lists:keysort( _Index=1, RightPoints )],
	reverse_and_drop_angle(L,[]);
					  
% Note that Y<=Yp by definition of the pivot, hence Y-Yp<=0
sort_by_angle( Pivot={Xp,Yp}, [Point={X,Y}|T], LeftPoints, MiddlePoint,
			   RightPoints ) ->
	case X-Xp of 
		
		0 ->
			% Here we are just above the pivot, tan(Pi/2) is infinite.
			case MiddlePoint of

				undefined ->
					% First found is first best:
					sort_by_angle( Pivot, T, LeftPoints, Point, RightPoints );

				{_Xm,Ym} ->
					
					case Y < Ym of

						true ->
					        % This point is above the previous highest middle point,
					        % previous middle point can be dropped on the floor:
							sort_by_angle( Pivot, T, LeftPoints, Point, 
										   RightPoints );

						false ->
					        % The current point can be dropped on the floor, as it is
					        % below the highest middle point:
							sort_by_angle( Pivot, T, LeftPoints, MiddlePoint, 
										   RightPoints )
								
					end
			end;

		DeltaX when DeltaX > 0 ->
			% This is a point on the right of the pivot, stores the tangent
			% of the angle the vector defined by the pivot and that point 
			% makes with the abscissa axis:
			sort_by_angle( Pivot, T, LeftPoints, MiddlePoint, 
						   [ {(Y-Yp)/DeltaX,Point} |RightPoints] );	 
		
		NegativeDeltaX ->
			% This is a point on the left of the pivot:
			sort_by_angle( Pivot, T, [ {(Y-Yp)/NegativeDeltaX,Point} |LeftPoints], 
						   MiddlePoint, RightPoints )

	end.

 

reverse_and_drop_angle( [], Acc ) ->
	Acc;

reverse_and_drop_angle( [{_Tangent,Point}|T], Acc ) ->
	reverse_and_drop_angle( T, [Point|Acc] ).





% Convex hull section.



% Computes the convex hull corresponding to the specified list of points.
compute_convex_hull( Points ) ->

	{Pivot,RemainingPoints} = linear_2D:find_pivot( Points ),

	case length(RemainingPoints) of 
		
		Len when Len < 2 ->
			throw( not_enough_point_for_convex_hull );

		_Other ->
			% We have at least 2 points in addition to the pivot.
			io:format( "Pivot is ~w, remaining points: ~w.~n", 
					   [Pivot,RemainingPoints] ),

			[P|T] = sort_by_angle( Pivot, RemainingPoints ),
 
			% Initially only the pivot is known to belong to the convex hull.
			% We also add the pivot to the end of the NextPoints list, so that
			% the hull can be closed.
			compute_graham_scan_hull( _CurrentlySelected=[Pivot], 
									  _StudiedPoint=P, _NextPoints=(T++[Pivot]) )

	end.




% Computes the Graham scan for the specified list of points, expected to be
% already sorted by increasing angle between the abscissa axis and the vector
% from the pivot to each of these points (i.e. in increasing order of the angle
% they and the point P make with the x-axis, in counter-clockwise order).
% See: http://en.wikipedia.org/wiki/Graham_scan
% Returns the corresponding convex hull, in clock-wise order.
compute_graham_scan_hull( CurrentlySelected, _Pivot, _NextPoints=[] ) ->
	% Last studied point is by construction always to pivot.

	io:format( "compute_graham_scan_hull: "
			   "exhausted input points, returning: ~w.~n", [CurrentlySelected] ),

	CurrentlySelected;

compute_graham_scan_hull( CurrentlySelected=[H|_T], StudiedPoint, 
					 [Next|OtherNext] ) ->

	case is_on_the_right( StudiedPoint, H, Next ) of

		true ->
			
			io:format( "compute_graham_scan_hull: point ~w is on the right of "
					   "segment from ~w to ~w, keeping ~w.~n", 
					   [StudiedPoint, H, Next, StudiedPoint ] ),
 
			% Here, the point 'Next' is on the right of the segment going from H
			% to studied point, thus H should not have been selected and is
			% discarded (next turns we might discard more elements from T):
			compute_graham_scan_hull( [StudiedPoint|CurrentlySelected], Next,
									   OtherNext );
		
		false ->

			io:format( "compute_graham_scan_hull: point ~w is on the left of "
					   "segment from ~w to ~w, eliminating ~w.~n", 
					   [StudiedPoint, H, Next, StudiedPoint ] ),

			% Here, the point 'Next' is on the left of the segment going from H
			% to studied point, thus for the moment we can keep H and that
			% studied point, until being possibly proved wrong later:
			compute_graham_scan_hull( CurrentlySelected, Next, OtherNext )

	end.
	
	
% Returns whether P is on the left of the segment going from P1 to P2. Should P
% be on that segment, then P will be considered as not being on the left: in the
% convex hull, only necessary points will be kept, i.e. no point on the boundary
% of the hull will be kept.
is_on_the_right( _P={X,Y}, _P1={X1,Y1}, _P2={X2,Y2} ) ->	
	DotProduct = (X2-X1)*(Y-Y1) - (Y2-Y1)*(X-X1),
	DotProduct > 0.







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


