% Gathering of various convenient facilities.
% See utils_test.erl for the corresponding test.

-module(utils).

% Creation date: July 1, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([get_timestamp/0,get_textual_timestamp/0,
	convert_to_filename/1,speak/1,notify_user/1,notify_user/2,
	get_image_file_gif/1,term_toString/1,wait_for_global_registration_of/1]).


-define(ResourceDir,"resources").


% Returns a tuple describing the current time. 
% Ex: {{2007,9,6},{15,9,14}}
get_timestamp() ->
	{erlang:date(),erlang:time()}.


% Returns a string corresponding to the current timestamp.
get_textual_timestamp() ->
	{{Year,Month,Day},{Hour,Minute,Second}} = get_timestamp(),
 	%io_lib:format( "[~p:~p:~p]", tuple_to_list(now()) ).
 	io_lib:format( "[~p/~p/~p ~B:~2..0B:~2..0B]",
		[Year,Month,Day,Hour,Minute,Second] ).
		
		
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
% @example 'utils:notify_user( "Hello ~w", [ Name ]).'
notify_user(Message,FormatList) ->
	ActualMessage = io_lib:format(Message,FormatList), 
	io:format(ActualMessage),
	speak(ActualMessage).


	
% Returns the image path corresponding to the specified file.	
get_image_file_gif(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".gif"]).


% Returns a human-readable string describing specified term.	
term_toString(Term) ->
	case io_lib:printable_list(Term) of
	
		true -> io_lib:format("~s",[Term]);
		
		_    -> io_lib:format("~p",[Term])
		
	end.	



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
	
