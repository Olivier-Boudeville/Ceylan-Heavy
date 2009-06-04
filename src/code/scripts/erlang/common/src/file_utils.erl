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
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regarding files.
% See file_utils_test.erl for the corresponding test.
-module(file_utils).


% Related standard modules: file, filename.




% Filename-related operations.

-export([ join/2, convert_to_filename/1, exists/1, get_type_of/1, is_file/1,
	is_directory/1, list_dir_elements/1, filter_by_extension/2,
	find_files_from/2, find_all_files_from/1, 
	path_to_variable_name/1, path_to_variable_name/2,
	get_image_file_png/1, get_image_file_gif/1 ]).



-export([ file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
	zipped_term_to_unzipped_file/2, files_to_zipped_term/1,
	zipped_term_to_unzipped_files/1 ]).



% For the file_info record:
-include_lib("kernel/include/file.hrl").




% Filename-related operations.


% Joins the two specified path elements.
% Note: join/2 added back to file_utils, filename:join(Components) or
% filename:join(Name1, Name2) can be used instead.
% However filename:join("","my_dir") results in "/my_dir", whereas often we
% would want "my_dir", which is returned by file_utils:join/2.
join( "", SecondPath ) ->
	SecondPath ;
	
join( FirstPath, SecondPath ) ->
	filename:join( FirstPath, SecondPath ).
	
	
		
% Converts specified name to an acceptable filename, filesystem-wise.	
convert_to_filename(Name) ->
	% Replaces all series of spaces by one underscore:
	re:replace( lists:flatten(Name), " +", "_", [global,{return, list}] ).



% Tells whether specified file entry exists, regardless of its type.
exists(EntryName) ->
	case file:read_file_info(EntryName) of 
	
		{ok,_FileInfo} ->
			true;
			
		{error,_Reason} ->
			false
			
	end.		

	
	
% Returns the type of the specified file entry, in:
% device | directory | regular | other.
get_type_of(EntryName) ->
	case file:read_file_info(EntryName) of 
	
		{ok,FileInfo} ->
			#file_info{ type = FileType } = FileInfo,
			FileType;
			
		{error,enoent} ->
			throw({non_existing_entry,EntryName})
			
	end.		



% Returns whether the specified entry is a regular file.
is_file(EntryName) ->
	case get_type_of(EntryName) of 
	
		regular ->
			true ;
			
		_ ->
			false	
	
	end.
		

		
% Returns whether the specified entry is a regular file.
is_directory(EntryName) ->
	case get_type_of(EntryName) of 
	
		directory ->
			true ;
			
		_ ->
			false	
	
	end.
		
		

% Returns a tuple made of a four lists describing the file elements found in
% specified directory: {RegularFiles,Directories,OtherFiles,Devices}.
list_dir_elements(Dirname) ->
	%io:format( "list_dir_elements for '~s'.~n", [Dirname] ),
	{ok,LocalDirElements} = file:list_dir(Dirname),
	classify_dir_elements( Dirname, LocalDirElements, [], [], [], [] ).	
	
	
	
% Returns a tuple containing four lists corresponding to the sorting of all 
% file elements: {Directories,RegularFiles,Devices,OtherFiles}.
classify_dir_elements( _Dirname, [], 
		Devices, Directories, RegularFiles, OtherFiles ) ->
	% Note the reordering: 
	{RegularFiles,Directories,OtherFiles,Devices};
	
classify_dir_elements( Dirname, [H|T], 
		Devices, Directories, RegularFiles, OtherFiles ) ->
		
	 case get_type_of( filename:join( Dirname, H ) ) of 
	 
	 	device ->
			classify_dir_elements( Dirname, T, 
				[H|Devices], Directories, RegularFiles, OtherFiles ) ; 
			
		directory ->
			classify_dir_elements( Dirname, T, 
				Devices, [H|Directories], RegularFiles, OtherFiles ) ; 
		
		regular ->
			classify_dir_elements( Dirname, T, 
				Devices, Directories, [H|RegularFiles], OtherFiles ) ; 
		
		other ->
			classify_dir_elements( Dirname, T, 
				Devices, Directories, RegularFiles, [H|OtherFiles] ) 

	end.
	
	
	
% Returns a list containing all elements of FilenameList whose extension is 
% the specified one.	
% Signature: filter_by_extension( FilenameList, Extension )
filter_by_extension( Filenames, Extension ) ->	
	filter_by_extension( Filenames, Extension, [] ).
		

filter_by_extension( [], _Extension, Acc ) ->
	Acc ;
	
filter_by_extension( [H|T], Extension, Acc ) ->	
	case filename:extension(H) of 
	
		Extension ->
			filter_by_extension( T, Extension, [H|Acc] ) ;	
		
		_Other ->
			filter_by_extension( T, Extension, Acc )	
				
	end.	 
		 



% Returns the list of all regular files found from the root with specified
% extension, in the whole subtree (i.e. recursively). 
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
find_files_from( RootDir, Extension ) ->
	find_files_from( RootDir, "", Extension, [] ).
	
	
find_files_from( RootDir, CurrentRelativeDir, Extension, Acc ) ->
	%io:format( "find_files_from in ~s.~n", [CurrentRelativeDir] ),
	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join(RootDir,CurrentRelativeDir) ),
	Acc ++ list_files_in_subdirs( Directories, Extension, 
			RootDir, CurrentRelativeDir, [] ) 
		++ prefix_files_with( CurrentRelativeDir,
			filter_by_extension(RegularFiles,Extension) ).
		
		
		
list_files_in_subdirs([],_Extension,_RootDir,_CurrentRelativeDir,Acc) -> 
	Acc;
	
list_files_in_subdirs([H|T],Extension,RootDir,CurrentRelativeDir,Acc) ->		
	list_files_in_subdirs( T, Extension, RootDir, CurrentRelativeDir,
		find_files_from( RootDir, join(CurrentRelativeDir,H), 
			Extension, [] ) ++ Acc ).




% Returns the list of all regular files found from the root, in the whole
% subtree (i.e. recursively). 
% All returned pathnames are relative to this root.
% Ex: [ "./a.txt", "./tmp/b.txt" ].
% All returned pathnames are relative to this root.
find_all_files_from( RootDir ) ->
	find_all_files_from( RootDir, "", [] ).
	
	
find_all_files_from( RootDir, CurrentRelativeDir, Acc ) ->
	%io:format( "find_all_files_from with root = '~s', current = '~s'.~n",
	%	[RootDir,CurrentRelativeDir] ),
	{RegularFiles,Directories,_OtherFiles,_Devices} = list_dir_elements(
		join(RootDir,CurrentRelativeDir) ),		
	Acc ++ list_all_files_in_subdirs( Directories, 
			RootDir, CurrentRelativeDir, [] ) 
		++ prefix_files_with( CurrentRelativeDir, RegularFiles ).
	
		 
		 
list_all_files_in_subdirs([],_RootDir,_CurrentRelativeDir,Acc) -> 
	Acc;
	
list_all_files_in_subdirs([H|T],RootDir,CurrentRelativeDir,Acc) ->		
	%io:format( "list_all_files_in_subdirs with root = '~s', current = '~s' "
	%	"and H='~s'.~n", [RootDir,CurrentRelativeDir,H] ),
	list_all_files_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_all_files_from( RootDir, join(CurrentRelativeDir,H), [] ) ++ Acc ).

		 
		 
prefix_files_with( RootDir, Files ) ->
	%io:format( "Prefixing ~p with '~s'.~n", [Files,RootDir] ),
	prefix_files_with( RootDir, Files, [] ).


prefix_files_with( _RootDir, [], Acc ) ->
	Acc;
	
prefix_files_with( RootDir, [H|T], Acc ) ->
	prefix_files_with( RootDir, T, [join(RootDir,H)|Acc] ).

		
		

% Converts specified path (full filename, like '/home/jack/test.txt' or
% './media/test.txt') into a variable name licit in most programming languages
% (ex: C/C++).
% Rule here is:
%  - variable name starts with a prefix, user-supplied or the default one
%  - any leading './' is removed
%  - '-' becomes '_'
%  - '.' becomes '_'
%  - '/' becomes '_'
path_to_variable_name(Filename) ->
	path_to_variable_name(Filename,"File_").
	

% Removes any leading './':
path_to_variable_name([$.,$/|T],Prefix) ->
	convert(T,Prefix);
	
path_to_variable_name(Filename,Prefix) ->
	convert(Filename,Prefix).
	
	
% Helper function.
convert(Filename,Prefix) ->	
	NoDashName = re:replace( lists:flatten(Filename), "-+", "_", 
		[global,{return, list}] ),
	NoDotName = re:replace( NoDashName, "\\.+", "_", 
		[global,{return, list}] ),	
	Prefix ++ re:replace( NoDotName, "/+", "_", 
		[global,{return, list}] ).
	


		 
-define(ResourceDir,"resources").
		
	
% Returns the image path corresponding to the specified file.	
get_image_file_png(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".png"]).



% Returns the image path corresponding to the specified file.	
get_image_file_gif(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".gif"]).




% Zip-related operations.
	
	
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
	
