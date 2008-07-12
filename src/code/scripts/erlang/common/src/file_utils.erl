% Gathering of various convenient facilities regarding files.
% See file_utils_test.erl for the corresponding test.
-module(file_utils).


% Creation date: Saturday, July 12, 2008.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([ convert_to_filename/1, generate_png_from_graph_file/2,
	generate_png_from_graph_file/3, display_png_file/1, get_image_file_png/1,
	get_image_file_gif/1, join/2,
	files_to_zipped_term/1, zipped_term_to_unzipped_files/1 ]).


-define(ResourceDir,"resources").

		
% Converts specified name to an acceptable filename, filesystem-wise.	
convert_to_filename(Name) ->
	% Replace spaces by underscores:
	{ok,Filename,_} = regexp:gsub(Name," ","_"),
	Filename.
	

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
	

% By default do not crash if dot outputs some warnings.
generate_png_from_graph_file(PNGFilename,GraphFilename) ->
	generate_png_from_graph_file(PNGFilename,GraphFilename,false).


% Displays (without blocing) to the user the specified PNG, using an external
% viewer. 
display_png_file(PNGFilename) ->
	% Viewer is 'eye of gnome' here (output ignored): 
	os:cmd( "eog " ++ PNGFilename ++ " &" ).
	
	
% Returns the image path corresponding to the specified file.	
get_image_file_png(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".png"]).


% Returns the image path corresponding to the specified file.	
get_image_file_gif(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".gif"]).

	
	
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation. 
% Inspired from http://www.trapexit.org/String_join_with.
join(_Separator,[]) ->
    "";

join(Separator,ListToJoin) ->
    lists:flatten( lists:reverse( join(Separator, ListToJoin, []) ) ).
	

join(_Separator,[],Acc) ->
    Acc;

join(_Separator,[H| [] ],Acc) ->
    [H|Acc];
	
join(Separator,[H|T],Acc) ->
    join(Separator, T, [Separator, H|Acc]).


% Reads in memory the files specified from their filename, zips the
% corresponding term, and returns it.
% Note: useful for network transfers of small files. 
% Larger ones should be transferred with TCP/IP and by chunks.
files_to_zipped_term(FilenameList) ->
	FileName = "dummy",
	%{ok,{_FileName,Bin}} = zip:zip( FileName, FilenameList, [verbose,memory] ),
	{ok,{_FileName,Bin}} = zip:zip( FileName, FilenameList, [memory] ),
	Bin.
	
zipped_term_to_unzipped_files(ZippedTerm) ->
	%zip:unzip(ZippedTerm,[verbose]).
	zip:unzip(ZippedTerm).


% Helper functions.

execute_dot(PNGFilename,GraphFilename) ->
	% Dot might issue non-serious warnings:
	os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename ).


