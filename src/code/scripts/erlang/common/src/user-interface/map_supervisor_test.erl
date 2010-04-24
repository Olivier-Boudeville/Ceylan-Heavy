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


% Unit tests for the map supervisor.
% See the map_supervisor.erl tested module.
-module(map_supervisor_test).


-export([ run/0 ]).

-define(Tested_module,map_supervisor).



% Implementation notes.


% There are virtual-world coordinates and screen coordinates.
%
% Virtual-world coordinates are expressed in an orthonormal basis, abscissa
% increasing onscreen from left to right, ordinate from bottom to top.
%
% Their unit is the virtual-world centimeter. This is not the meter, in order to
% stays as much as possible with integer coordinates. Supposing an integer is
% actually a signed 32 bit integer, coordinates will range in −2,147,483,648 to
% +2,147,483,647, which means a maximum length of 4,294,967,295 cm, thus 42 949
% 673 meters, about 43 kilometers. Therefore, in 32 bit, as long as distances
% will stay within that limit, computations will be effective.
% In 64 bit, distance is 18,446,744,073,709,551,615 cm, thus about
% 184 467 440 737 095 km, which is quite a lot.
%
% Screen coordinates are expressed in an orthonormal basis too, abscissa
% increasing onscreen from left to right, ordinate from top to bottom.
%
% Their unit is the pixel.


% The active map is a rectangular window on the screen which is opened on the
% virtual world.
% 
% Its onscreen size is determined by the layout and size of its containing
% window.
%
% This map is centered on MapOrigin={Xm,Ym}, expressed in virtual-world
% coordinates, thus in centimeters. 
%
% A zoom factor F is defined. It allows to convert screen distances into
% virtual-world distances. It is defined in virtual world centimeters per pixel.
% For example if F = 100 cm/px then a line segment of 10 pixels corresponds to 1
% meter in the virtual world.
% 
% The onscreen size of the map and the zoom factor determine how much of the
% virtual world is shown.


% Describes the state of a map.
-record( map_state, {

		   % The virtual-world coordinates of the map center, a pair of signed
		   % integers.
		   % Starts at the virtual world origin.
		   map_center = {0,0},

		   % The zoom factor F, in virtual world centimeters per pixel.
           % Starts with 1 meter corresponding to 1 pixel.
		   zoom_factor = 100,

		   % ID of  the main window:
		   main_win_id,

		   % ID of the canvas:
		   canvas_id
		  } ).



get_default_main_window_width() ->
	800.

get_default_main_window_height() ->
	600.


get_canvas_width() ->
	640.

get_canvas_height() ->
	480.



% Initializes once for all the GUI of the map supervisor.
init_supervisor_gui( State ) ->

	WindowSize = [ {width,get_default_main_window_width()}, 
				   {height,get_default_main_window_height()} ],

	GsId = gs:start(),

	MainWin = gs:window( GsId, WindowSize ++ [  
								{title,"Map Supervisor"},
								{bg,gui:get_color(grey)} ]),
	
	% Defines the packer:
	_Packer = gs:frame( main_packer, MainWin, [
					{packer_x,[]},
					{packer_y,[]}
											   ] ),


	Canvas = gs:create( canvas, MainWin, [ 
		{hscroll,bottom}, 
		{vscroll,left},
	    {width,get_canvas_width()},
		{height,get_canvas_height()},
		% Centers canvas:
	    {x,(get_default_main_window_width()-get_canvas_width()) / 2},
		{y,(get_default_main_window_height()-get_canvas_height()) / 2},
        {bg,gui:get_color(lime)}
        %{scrollregion, {100,200,30,200}}
										 ] ),

	gs:create( button, add_point_button, MainWin, [{width,75},{y,60},{x,10},
                             {width,100}, {label,{text,"Add point"}}]),

	InitialPointCount = 3,


    % Sets the GUI to visible:
	gs:config( MainWin, {map,true} ),

	%gui_main_loop( M ),

	gs:stop().

 

gui_main_loop( MainWin ) ->
	
	%io:format( "~nEntering main loop, point count is ~B.~n", [PointCount-1] ),

	receive
        
		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting GUI test.~n" ),
			erlang:halt();
		
		to_do ->
			%create_basic_test_gui( Canvas ),
			gui_main_loop( MainWin );

		{gs,add_point_button,click,[],[_Label]} ->
			%gs:destroy( Canvas ),
			%NewCanvas = create_test_gui( PointCount, MainWin ),
			gui_main_loop( MainWin );

		X ->
            io:format("GUI test got event '~w' (ignored).~n",[X]),
			%gs:destroy( Canvas ),
			%NewCanvas = create_test_gui( PointCount, MainWin ),
			gui_main_loop( MainWin )
	
	end.
	


run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	case init:get_argument('-batch') of
	
		{ok,_} ->
			io:format( "(not running the GUI test, being in batch mode)~n" );
		
		_ ->
			ok
			%init_test_gui()
			
	end,
		  
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

