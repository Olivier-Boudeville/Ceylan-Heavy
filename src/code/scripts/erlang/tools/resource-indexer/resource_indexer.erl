% Indexes resources.
-module(resource_indexer).


-export([main/0]).


% Describes a file entry corresponding to a resource.
-record( file_entry,
	{
		base_path,
		file_name,
		variable_name,
		content_type,
		resource_id
	}).



main() ->
	%io:format( "Arguments: ~p.", [init:get_plain_arguments()] ),
	ScanDir = manage_scan_dir(),
	IndexBasename = manage_index_basename(),
	Files = file_utils:find_all_files_from( ScanDir ),
	%io:format( "Found files: ~p.~n", [Files] ),
	FileEntries = create_entries_from( Files ),
	%io:format( "Corresponding resource entries:~n ~p~n.", [FileEntries] ),
	write_resources_as_xml( forge_xml_filename(IndexBasename), FileEntries ),
	write_resources_as_header_file( forge_header_filename(IndexBasename),
		FileEntries ),
	halt(0).


forge_xml_filename( IndexBasename ) ->
	file_utils:convert_to_filename( IndexBasename ) ++ ".xml".
	
	
forge_header_filename( IndexBasename ) ->
	file_utils:convert_to_filename( IndexBasename ) ++ ".h".
	
	
	
% Reads, checks and returns the proper directory from which resources should
% be scanned.
manage_scan_dir() ->
	ScanRootDir = case init:get_argument(scan_dir) of
	
		{ok,[[Dir]]} ->
			io:format( "  Will scan resources from specified "
				"resource directory '~s'.\n", [Dir] ),
			Dir;
			
		error ->	
			{ok,Dir} = file:get_cwd(),
			io:format( "  Will scan resources from current directory ('~s').\n",
				[Dir]),
			Dir;
		
		{ok,[Other]} ->
			handle_error( io_lib:format( 
				"incorrect scan_dir option: ~p", [Other] ) )
				
	end,
	
	case file_utils:exists(ScanRootDir) of
	
		true ->
			ok ;
		
		false ->
			handle_error( io_lib:format( "'~s' does not exist",
				[ScanRootDir] ) )
				
	end,
	
	case file_utils:is_directory(ScanRootDir) of
	
		true ->
			ok ;
		
		false ->
			handle_error( io_lib:format( "'~s' is not a directory",
				[ScanRootDir] ) )
				
	end,
	ScanRootDir.


      
% Determines the basename that should be used to manage an index.
% Returns that basename.
manage_index_basename() ->
	case init:get_argument(index_basename) of
	
		{ok,[[CommandLineBasename]]} ->
			io:format( "  Will use specified index basename, '~s'.\n",
				[CommandLineBasename] ),
			CommandLineBasename;	
						
		error ->
			DefaultBasename = "resource-map",
			io:format( "  Will use default index basename, '~s'.\n",
				[DefaultBasename] ),
			DefaultBasename;	
		
		{ok,[Other]} ->
			handle_error( io_lib:format( 
				"incorrect index_basename option: ~p", [Other] ) )
				
	end.



% Returns an atom describing the type of specified file.
% General content types (ex: 'audible') have to be defined instead of more
% precise ones (ex: 'sound', 'music') as a given extension (ex: ".mp3") can 
% be used in several contexts (ex: for musics or for sounds).
get_content_type( Filename ) ->
	case filename:extension( Filename ) of
	
		".png" ->
			image;
			
		".jpeg" ->
			image;

		".jpg" ->
			image;
			
		".mp3" ->
			audible;
			
		".ogg" ->
			audible;

		".wav" ->
			audible;
				
		_Other->
			unknown
			
	end.
	 


create_entries_from( Files ) ->
	create_entries_from( Files, [], 1 ).
	
	
create_entries_from( [], Entries, _CurrentId ) ->
	Entries ;
	
create_entries_from( [Filename|T], Entries, CurrentId ) ->
	NewEntry = #file_entry
	{
		base_path = filename:dirname(Filename),
		file_name = filename:basename(Filename),
		variable_name = file_utils:path_to_variable_name(Filename),
		content_type = get_content_type(Filename),
		resource_id = CurrentId 
	},
	create_entries_from( T, [ NewEntry | Entries], CurrentId +1 ).


	
% Writes specified file entries into specified XML filename.	
write_resources_as_xml( XMLTargetFilename, FileEntries ) ->
	io:format( "  Writing XML resource file '~s'.~n", [XMLTargetFilename] ),
	{ok,File} = file:open( XMLTargetFilename, [write]),	
	write_xml_file_entries_header( File ),
	write_xml_file_entries( File, lists:reverse(FileEntries) ),
	write_xml_file_entries_footer( File ).
	
	
write_xml_file_entries_header( File ) ->
	io:format( File, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~n", [] ),
	io:format( File, "<resource_list>~n~n", [] ).

	 
	 
write_xml_file_entries( _File, [] ) ->
	ok;
	
write_xml_file_entries( File, [H|T] ) ->
	io:format( File, "  <resource id=\"~B\">~n", [H#file_entry.resource_id] ),
	%io:format( File, "    <base_path=\"~s\">~n", [H#file_entry.base_path] ),
	%io:format( File, "    <file_name=\"~s\">~n", [H#file_entry.file_name] ),
	io:format( File, "    <resource_path=\"~s\">~n", 
		[filename:join(H#file_entry.base_path,H#file_entry.file_name)] ),
	%io:format( File, "    <variable_name=\"~s\">~n",
	%	[H#file_entry.variable_name] ),
	io:format( File, "    <content_type=\"~w\">~n", 
		[H#file_entry.content_type] ),
	io:format( File, "  </resource>~n~n", [] ),
	write_xml_file_entries( File, T ).
	

	
write_xml_file_entries_footer( File ) ->	 
	io:format( File, "</resource_list>~n~n", [] ).



% Writes specified file entries into specified XML filename.	
write_resources_as_header_file( HeaderTargetFilename, FileEntries ) ->
	io:format( "  Writing header resource file '~s'.~n", 
		[HeaderTargetFilename] ),
	{ok,File} = file:open( HeaderTargetFilename, [write]),
	DefineSymbol = get_define_for(HeaderTargetFilename),		
	write_header_file_entries_header( File, DefineSymbol ),
	write_header_file_entries( File, lists:reverse(FileEntries) ),
	write_header_file_entries_footer( File, DefineSymbol ).
	
	
write_header_file_entries_header( File, DefineSymbol ) ->
	io:format( File, "#ifndef ~s~n", [DefineSymbol] ),
	io:format( File, "#define ~s~n~n~n", [DefineSymbol] ),
	io:format( File, "#include \"Resource.h\"~n~n~n", [] ),
	io:format( File, "namespace Resource~n", [] ),
	io:format( File, "{~n~n", [] ).
	
	
	 
write_header_file_entries( _File, [] ) ->
	ok;
	
write_header_file_entries( File, [H|T] ) ->
	io:format( File, "  static const ResourceID ~s = ~B;~n", 
		[H#file_entry.variable_name,H#file_entry.resource_id] ),
	write_header_file_entries( File, T ).
	

	
write_header_file_entries_footer( File, DefineSymbol ) ->	 
	io:format( File, "~n}~n~n~n", [] ),
	io:format( File, "#endif // ~s~n~n", [DefineSymbol] ).


get_define_for( Filename ) ->
	get_define_for( Filename, [] ).
	
	
get_define_for( [], Acc ) ->
	lists:reverse(Acc);
	
get_define_for( [$.|T], Acc ) ->
	get_define_for( T, [$_|Acc] );
	
get_define_for( [H|T], Acc ) ->
	get_define_for( T, [string:to_upper(H)|Acc] ).



display_usage() ->
    io:format("Usage: resource_indexer.sh [-scan_dir A_PATH] \n"
		"Index all resources from scan directory, "
		"which is by default the current directory.\n"
		"  Options are:\n"
		"     -scan_dir A_PATH: scans from the A_PATH directory").



handle_error( Message ) ->
	% Not able to get all raw arguments:
	%io:format( "  Error, ~s. Specified parameters were: '~p'.\n", 
	%	[Message,init:get_plain_arguments()] ),
	io:format( "\n  Error, ~s.\n", [Message] ),
	display_usage(),
	halt(1).

