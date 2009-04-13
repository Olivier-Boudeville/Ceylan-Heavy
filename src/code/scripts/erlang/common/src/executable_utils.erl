% 
% Copyright (C) 2003-2009 Olivier Boudeville
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


% Gathering of various convenient facilities regarding executing of third-party
% tools.
% See executable_utils_test.erl for the corresponding test.
-module(executable_utils).


% Creation date: Saturday, July 12, 2008.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([ generate_png_from_graph_file/2,
	generate_png_from_graph_file/3, display_png_file/1 ]).

	
% By default do not crash if dot outputs some warnings.
generate_png_from_graph_file(PNGFilename,GraphFilename) ->
	generate_png_from_graph_file(PNGFilename,GraphFilename,false).


% Generates a PNG image file from specified graph file, that must respect the
% dot (graphviz) syntax.
%  - PNGFilename the filename of the PNG to generate
%  - GraphFilename the filename corresponding to the source graph
%  - HaltOnDotOutput tells whether the process should crash if dot outputs
% a warning
generate_png_from_graph_file(PNGFilename,GraphFilename,true) ->
	[] = execute_dot(PNGFilename,GraphFilename);

% Any output remains available to the caller.
generate_png_from_graph_file(PNGFilename,GraphFilename,false) ->
	execute_dot(PNGFilename,GraphFilename).
	



% Displays (without blocking) to the user the specified PNG, using an external
% viewer. 
display_png_file(PNGFilename) ->
	% Viewer is 'eye of gnome' here (its output is ignored): 
	os:cmd( "eog " ++ PNGFilename ++ " &" ).
	

% Helper functions.

execute_dot(PNGFilename,GraphFilename) ->
	% Dot might issue non-serious warnings:
	os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename ).

