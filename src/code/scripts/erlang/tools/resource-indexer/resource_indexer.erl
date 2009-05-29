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
	Files = file_utils:find_all_files_from( ScanDir ),
	io:format( "Found files: ~p.", [Files] ),
	FileEntries = create_entries_from( Files ),
	io:format( "Corresponding resource entries: ~p~n.", [FileEntries] ),
	halt(0).


	
% Reads, checks and returns the proper directory from which resources should
% be scanned.
manage_scan_dir() ->
	ScanRootDir = case init:get_argument(scan_dir) of
	
		{ok,[[Dir]]} ->
			io:format( 
				"Will scan resources from specified resource directory '~s'.\n",
				[Dir] ),
			Dir;
			
		error ->	
			{ok,Dir} = file:get_cwd(),
			io:format( "Will scan resources from current directory ('~s').\n",
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

