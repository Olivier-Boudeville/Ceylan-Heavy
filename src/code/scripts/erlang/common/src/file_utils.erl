% Gathering of various convenient facilities regarding files.
% See file_utils_test.erl for the corresponding test.
-module(file_utils).


% Creation date: Saturday, July 12, 2008.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


% Note: join/2 removed from file_utils, use filename:join(Components) or
% filename:join(Name1, Name2) instead.


-export([ convert_to_filename/1, get_image_file_png/1, get_image_file_gif/1,
	file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
	zipped_term_to_unzipped_file/2, files_to_zipped_term/1,
	zipped_term_to_unzipped_files/1 ]).


-define(ResourceDir,"resources").


		
% Converts specified name to an acceptable filename, filesystem-wise.	
convert_to_filename(Name) ->
	% Replace spaces by underscores:
	{ok,Filename,_} = regexp:gsub( lists:flatten(Name)," ","_"),
	%io:format( "Converted '~s' into '~s'.~n", [Name,Filename] ),
	Filename.
		
	
% Returns the image path corresponding to the specified file.	
get_image_file_png(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".png"]).


% Returns the image path corresponding to the specified file.	
get_image_file_gif(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".gif"]).

	
	
% Reads in memory the file specified from its filename, zips the
% corresponding term, and returns it.
% Note: useful for network transfers of small files. 
% Larger ones should be transferred with TCP/IP and by chunks.
% Returns a binary.
file_to_zipped_term(Filename)  ->
	DummyFileName = "dummy",
	%{ok,{_DummyFileName,Bin}} = zip:zip( DummyFileName, Filename,
	% [verbose,memory] ),
	{ok,{_DummyFileName,Bin}} = 
		%zip:zip( DummyFileName, [Filename], [verbose,memory] ),
		zip:zip( DummyFileName, [Filename], [memory] ),
	Bin.
	
	
% Reads specified binary, extracts the zipped file in it and writes it
% on disk, in current directory.
% Returns the filename of the unzipped file.	
zipped_term_to_unzipped_file(ZippedTerm) ->
	%zip:unzip(ZippedTerm,[verbose]).
	{ok,[FileName]} = zip:unzip(ZippedTerm),
	FileName.
	
	
% Reads specified binary, extracts the zipped file in it and writes it
% on disk, in current directory, under specified filename instead of under
% filename stored in the zip archive.
% Any pre-existing file will be overwritten.
% Note: only one file is expected in the specified archive.
zipped_term_to_unzipped_file(ZippedTerm,TargetFilename) ->
	{ok,[{_AFilename, Binary}]} = zip:unzip(ZippedTerm,[memory]),
	{ok,File} = file:open( TargetFilename, [write] ),
	ok = io:format( File, "~s", [ binary_to_list(Binary) ] ),
	ok = file:close(File).
	



% Reads in memory the files specified from their filename, zips the
% corresponding term, and returns it.
% Note: useful for network transfers of small files. 
% Larger ones should be transferred with TCP/IP and by chunks.
% Returns a binary.
files_to_zipped_term(FilenameList) when is_list(FilenameList) ->
	DummyFileName = "dummy",
	%{ok,{_DummyFileName,Bin}} = zip:zip( DummyFileName, FilenameList,
	% [verbose,memory] ),
	{ok,{_DummyFileName,Bin}} = 
		zip:zip( DummyFileName, FilenameList, [memory] ),
	Bin.
	

% Reads specified binary, extracts the zipped files in it and writes them
% on disk, in current directory.	
% Returns a list of the filenames of the unzipped files.	
zipped_term_to_unzipped_files(ZippedTerm) ->
	%zip:unzip(ZippedTerm,[verbose]).
	{ok,FileList} = zip:unzip(ZippedTerm),
	FileList.
	
