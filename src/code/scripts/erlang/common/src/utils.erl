% Gathering of various convenient facilities.
% See utils_test.erl for the corresponding test.
-module(utils).


% Creation date: July 1, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([get_timestamp/0,get_textual_timestamp/0,get_textual_timestamp/1,
	get_duration/2,get_textual_duration/2,
	convert_to_filename/1,speak/1,notify_user/1,notify_user/2,
	generate_png_from_graph_file/2,generate_png_from_graph_file/3,
	display_png_file/1,
	get_image_file_png/1,get_image_file_gif/1,
	term_toString/1,term_to_string/1,
	integer_to_string/1,
	register_as/2,register_as/3,wait_for_global_registration_of/1,
	join/2,get_current_erlang_version/0]).


-define(ResourceDir,"resources").


% Returns a tuple describing the current time. 
% Ex: {{2007,9,6},{15,9,14}}
get_timestamp() ->
	{erlang:date(),erlang:time()}.


% Returns a string corresponding to the current timestamp.
get_textual_timestamp() ->
	get_textual_timestamp( get_timestamp() ).
	
get_textual_timestamp({{Year,Month,Day},{Hour,Minute,Second}}) ->
 	io_lib:format( "~p/~p/~p ~B:~2..0B:~2..0B",
		[Year,Month,Day,Hour,Minute,Second] ).


% Returns the duration in seconds between the two specified timestamps.	
get_duration(FirstTimestamp,SecondTimestamp) ->
	First  = calendar:datetime_to_gregorian_seconds(FirstTimestamp),
	Second = calendar:datetime_to_gregorian_seconds(SecondTimestamp),
	Second - First.


% Returns a textual description of the duration between the two specified
% timestamps.	
get_textual_duration(FirstTimestamp,SecondTimestamp) ->
	{Days,{Hour, Minute, Second}} = calendar:seconds_to_daystime( 
		get_duration(FirstTimestamp,SecondTimestamp) ),
	lists:flatten( io_lib:format( "~B day(s), ~B hour(s), ~B minute(s) "
		"and ~B second(s)", [Days, Hour, Minute, Second] ) ).
		
		
% Converts specified name to an acceptable filename, filesystem-wise.	
convert_to_filename(Name) ->
	% Replace spaces by underscores:
	{ok,Filename,_} = regexp:gsub(Name," ","_"),
	Filename.


% Speaks the specified message, using espeak.		
speak(Message) ->
	[] = os:cmd("espeak -s 140 \"" ++ Message ++ "\" &" ).
	

% Notifies the user of the specified message, with log output and synthetic
% voice.			
notify_user(Message) ->
	io:format(Message),
	speak(Message).


% Notifies the user of the specified message, with log output and synthetic
% voice.		
%Example: 'utils:notify_user( "Hello ~w", [ Name ]).'
notify_user(Message,FormatList) ->
	ActualMessage = io_lib:format(Message,FormatList), 
	io:format(ActualMessage),
	speak(ActualMessage).


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



% Returns a human-readable string describing specified term.	
term_toString(Term) ->
	case io_lib:printable_list(Term) of
	
		true -> io_lib:format("~s",[Term]);
		
		_    -> io_lib:format("~p",[Term])
		
	end.	


term_to_string(Term) ->
	term_toString(Term).
	
	
% Avoids to have to use lists:flatten when converting an integer to a string.	
integer_to_string(IntegerValue) ->
	hd( io_lib:format( "~B", [IntegerValue] ) ).
	
	
% Registers the current process under specified name.
% Declaration is register_as(ServerName,RegistrationType) with 
% RegistrationType in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested.
% Returns ok on success.
% If local registration fails, local_registration_failed is returned.
% If global registration fails, global_registration_failed is returned.
register_as(ServerName,RegistrationType) ->
	register_as( self(), ServerName, RegistrationType ).


% Registers specified PID under specified name.
% Declaration is: register_as(Pid,ServerName,RegistrationType) with 
% RegistrationType is in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested.
% Returns ok on success.
% If local registration fails, local_registration_failed is returned.
% If global registration fails, global_registration_failed is returned.
register_as(Pid,ServerName,local_only) ->
	case erlang:register( ServerName, Pid ) of 
	
		true ->
			ok;
			
		false ->
			local_registration_failed				
	
	end;
 
register_as(Pid,ServerName,global_only) ->
	case global:register_name( ServerName, Pid ) of 
	
		yes ->
			ok;
					
		no ->
			global_registration_failed,ServerName				
			
	end;

register_as(Pid,ServerName,local_and_global) ->
	ok = register_as(Pid,ServerName,local_only),
	ok = register_as(Pid,ServerName,global_only).


% Waits (up to 5 seconds) until specified name is globally registered.
% Returns either the resolved Pid or {registration_waiting_timeout,Name}.
wait_for_global_registration_of(Name) ->
	wait_for_global_registration_of(Name,5).
	

wait_for_global_registration_of(Name,0) ->
	{registration_waiting_timeout,Name};
	
wait_for_global_registration_of(Name,SecondsToWait) ->
	case global:whereis_name( Name ) of 
	
		undefined ->
			timer:sleep(1000),
			wait_for_global_registration_of(Name,SecondsToWait-1);
				
		Pid ->
			Pid
			
	end.
	
	
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation. 
% Inspired from http://www.trapexit.org/String_join_with.
join(Separator,ListToJoin) ->
    lists:flatten( lists:reverse( join(Separator, ListToJoin, []) ) ).

join(_Separator,[],Acc) ->
    Acc;

join(_Separator,[H| [] ],Acc) ->
    [H|Acc];
	
join(Separator,[H|T],Acc) ->
    join(Separator, T, [Separator, H|Acc]).


% Returns the version informations of the current Erlang interpreter being used.
get_current_erlang_version() ->
	erlang:system_info(otp_release).


% Helper functions.

execute_dot(PNGFilename,GraphFilename) ->
	% Dot might issue non-serious warnings:
	os:cmd( "dot -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename ).


