% Gathering of various convenient facilities.
% See basic_utils_test.erl for the corresponding test.
-module(basic_utils).


% Creation date: July 1, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


% Note that string:tokens can be used to split strings.

-export([ get_timestamp/0, get_textual_timestamp/0, get_textual_timestamp/1,
	timestamp_to_string/1, get_duration/2, get_textual_duration/2,
	speak/1, notify_user/1, notify_user/2,
	term_toString/1, term_to_string/1, integer_to_string/1,
	ipv4_to_string/1, ipv4_to_string/2,join/2,
	register_as/2, register_as/3, wait_for_global_registration_of/1,
	start_random_source/0, stop_random_source/0, get_random_value/1,
	get_random_module_name/0, checkpoint/1,
	get_interpreter_version/0 ]).



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

% Alias of get_textual_timestamp.
timestamp_to_string(Timestamp) ->	
	get_textual_timestamp(Timestamp).
	
	
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
% Example: 'basic_utils:notify_user( "Hello ~w", [ Name ]).'
notify_user(Message,FormatList) ->
	ActualMessage = io_lib:format(Message,FormatList), 
	io:format(ActualMessage),
	speak(ActualMessage).


% Returns a human-readable string describing specified term.	
term_toString(Term) ->
	case io_lib:printable_list(Term) of
	
		true -> io_lib:format("~s",[Term]);
		
		_    -> io_lib:format("~p",[Term])
		
	end.	


term_to_string(Term) ->
	term_toString(Term).
	
	
% Avoids to have to use lists:flatten when converting an integer to a string.
% Useless when using functions like io:format that accept iolists as 
% parameters.	
integer_to_string(IntegerValue) ->
	hd( io_lib:format( "~B", [IntegerValue] ) ).
	
	
ipv4_to_string( {N1,N2,N3,N4} ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B", [N1,N2,N3,N4] ) ).
	
	
ipv4_to_string( {N1,N2,N3,N4}, Port ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B:~B", [N1,N2,N3,N4,Port] ) ).


% Python-like 'join', combines items in a list into a string using a separator
% between each item representation. 
% Inspired from http://www.trapexit.org/String_join_with.
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
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
	ok = register_as(Pid,ServerName,global_only);

register_as(_Pid,_ServerName,none) ->
	ok.


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




% Random section.

% If use_crypto_module is defined, the crypto module will be used, otherwise
% the random module will be used instead.
%-define(use_crypto_module,).


-ifdef(use_crypto_module).


% crypto module used:

start_random_source() ->
	ok = crypto:start().


stop_random_source() ->
	ok = crypto:stop().


% Returns an integer random value generated from an uniform distribution. 
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
get_random_value(N) ->
	crypto:rand_uniform(1,N+1).


get_random_module_name() ->
	crypto.
	
	
-else. % use_crypto_module


% Default random module used:

start_random_source() ->
	ok.
	
	
stop_random_source() ->
	ok.
	
	
% Returns an integer random value generated from an uniform distribution. 
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
get_random_value(N) ->
	random:uniform(N).

	
get_random_module_name() ->
	random.
	
	
-endif. % use_crypto_module



% Displays a numbered checkpoint. 
% Useful for debugging purposes.
checkpoint(Number) ->
	io:format( "----- CHECKPOINT #~B -----~n", [Number] ).
	
	
	
% Returns the version informations of the current Erlang interpreter being used.
get_interpreter_version() ->
	erlang:system_info(otp_release).


% Helper functions.

