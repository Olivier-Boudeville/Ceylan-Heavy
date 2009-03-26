% Gathering of various convenient facilities.
% See basic_utils_test.erl for the corresponding test.
-module(basic_utils).


% Creation date: July 1, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


% Note that string:tokens can be used to split strings.


% Timestamp-related functions.
-export([ get_timestamp/0, get_textual_timestamp/0, get_textual_timestamp/1,
	timestamp_to_string/1, get_duration/2, get_textual_duration/2 ]).
	
	
% String management functions.
-export([ term_toString/1, term_to_string/1, integer_to_string/1,
	ipv4_to_string/1, ipv4_to_string/2, join/2 ]).
	
	
% Registration functions.
-export([ register_as/2, register_as/3, wait_for_global_registration_of/1,
	wait_for_local_registration_of/1 ]).


% Random functions.
-export([ start_random_source/3, start_random_source/1, stop_random_source/0,
	get_random_value/1,	get_random_module_name/0, get_random_seed/0,
	random_select/2, random_permute/1 ]).
	

% List management functions.
-export([ get_element_at/2, remove_element_at/2, subtract_all_duplicates/2 ]). 


% User-related functions.
-export([ speak/1, notify_user/1, notify_user/2 ]).


% Node-related functions.
-export([ generate_valid_node_name_from/1, check_node_validity/1 ]). 


% Miscellaneous functions.
-export([ generate_basic_name_from/1, flush_pending_messages/0, checkpoint/1,
	get_interpreter_version/0 ]).





% Timestamp-related functions.


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
		
		

		
% String management functions.
		

% Returns a human-readable string describing specified term.	
term_toString(Term) ->
	case io_lib:printable_list(Term) of
	
		true -> 
			io_lib:format("~s",[Term]);
		
		_    ->
			io_lib:format("~p",[Term])
		
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


% join(Separator,ListToJoin), ex: join( '-', [ "Barbara", "Ann" ] ).
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation. 
% Inspired from http://www.trapexit.org/String_join_with.
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
% Note: use string:tokens to split the string.
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


	

% Registration functions.
	
	
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
% Returns either the resolved Pid or {global_registration_waiting_timeout,Name}.
wait_for_global_registration_of(Name) ->
	wait_for_global_registration_of(Name,5).
	

wait_for_global_registration_of(Name,0) ->
	{global_registration_waiting_timeout,Name};
	
wait_for_global_registration_of(Name,SecondsToWait) ->
	case global:whereis_name( Name ) of 
	
		undefined ->
			timer:sleep(1000),
			wait_for_global_registration_of(Name,SecondsToWait-1);
				
		Pid ->
			Pid
			
	end.




% Waits (up to 5 seconds) until specified name is locally registered.
% Returns either the resolved Pid or {local_registration_waiting_timeout,Name}.
wait_for_local_registration_of(Name) ->
	wait_for_local_registration_of(Name,5).
	

wait_for_local_registration_of(Name,0) ->
	{local_registration_waiting_timeout,Name};
	
wait_for_local_registration_of(Name,SecondsToWait) ->
	case erlang:whereis( Name ) of 
	
		undefined ->
			timer:sleep(1000),
			wait_for_local_registration_of(Name,SecondsToWait-1);
				
		Pid ->
			Pid
			
	end.




% Random functions.

% If use_crypto_module is defined, the crypto module will be used, otherwise
% the random module will be used instead.
%
% Currently the crypto module is not used, as:
%  - not all Erlang VM can be built with the proper SSH support
%  - it is unclear whether the crypto module can be seeded like the random
% module can be (probably it cannot be)
%
% Therefore the two modules are not interchangeable.
%
%-define(use_crypto_module,).


-ifdef(use_crypto_module).


% crypto module used here.
% The seed and state management is presumably global (not per-process).

start_random_source(_A,_B,_C) ->
	throw(crypto_module_cannot_be_seeded).


start_random_source( default_seed ) ->
	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw(crypto_module_cannot_be_seeded).


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


% Default random module used here.
% The seed and state management is per-process (stored in the process
% dictionary).

start_random_source(A,B,C) ->
	random:seed(A, B, C).


% Seeds the random number generator, either with a default seed (if wanting to
% obtain the same random series at each run) or with current time (if wanting
% "real" non-reproducible randomness).
start_random_source( default_seed ) ->
	% Use default (fixed) values in the process dictionary:
	random:seed();

start_random_source( time_based_seed ) ->
	% Each run will result in different random series:
	{A, B, C} = erlang:now(),
	start_random_source(A,B,C).
	
	
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



-define(seed_upper_bound,65500).

% Returns a seed obtained from the random source in use.
% This is a randomly-determined seed, meant to be used to create another
% random generator.
get_random_seed() ->
	{   get_random_value( ?seed_upper_bound ), 
		get_random_value( ?seed_upper_bound ), 
		get_random_value( ?seed_upper_bound ) }.
	

% Returns a random uniform permutation of the specified list.
% Inspired from http://paste.lisp.org/display/74804.
% All these algorithms would need random access to a list, which is not
% readily possible here, hence must be emulated.
random_select( _, 0 ) -> 
	[];
	
random_select( List, N ) -> 
	% Uses the 'random' basic random source:
	Index = random:uniform( length(List) ),
	[ get_element_at( List, Index ) 
		| random_select( remove_element_at( List, Index ), N-1 ) ].

random_permute( List ) ->
	random_select( List, length(List) ).
	




% List management functions.


% Index start at position #1, not #0.

% Returns the element in the list at the specified index, in [1..length(List)].
% If the index is out of bounds, a function_clause like 
% '[{basic_utils,get_element_at,...}]' is triggered.
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
% Signature: get_element_at(List,Index)
get_element_at( List, 1 ) ->
	hd(List);

get_element_at( [_H|T], Index ) ->	
	get_element_at( T, Index-1 ).
		

% Returns a list corresponding to the specified one with the element
% at specified index removed.
% If the index is out of bounds, a function_clause like 
% '[{basic_utils,remove_element_at,...}]' is triggered.
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
% Signature: remove_element_at(List,Index) ->
% Not tail recursive version:
%remove_element_at( [_H|T], 1 ) -> 
%	T;
%	
%remove_element_at( [H|T], N ) ->
%	[H|remove_element_at(T,N-1)].
% Tail recursive version:
remove_element_at(List,Index) ->
	remove_element_at(List,Index,[]).

remove_element_at([_H|RemainingList],1,Result) ->
	lists:reverse( Result ) ++ RemainingList;
	 
remove_element_at([H|RemainingList],Index,Result) ->
	remove_element_at(RemainingList,Index-1,[H|Result]).


% Returns a list equal to L1 except that all elements found in L2 have been
% removed, even if in L1 they were duplicated.
% Note: like lists:subtract, except that all occurences from L2 (not only
% the first one) are removed.
% Example: [1,4] = basic_utils:subtract_all_duplicates( [1,2,3,4,2], [2,3] )  
% Taken from
% http://www.trapexit.org/Finding_Elements_in_One_Array_but_Not_Another
subtract_all_duplicates( L1, L2 ) ->
	lists:filter( fun(E) -> not lists:member(E,L2) end, L1).





% User-related functions.


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





% Node-related functions.


% Returns a name that is a legal name for an Erlang node, forged from
% specified one.
generate_valid_node_name_from( Name ) when is_list(Name) ->
	{ok,ForgedName,_} = regexp:gsub( lists:flatten(Name),
	 	" ","_" ),
	ForgedName.


% Returns true iff the specified node is available, Erlang-wise.
check_node_validity( Node ) when is_list(Node) ->
	% ping requires atoms:
	check_node_validity( list_to_atom(Node) ) ;
	
check_node_validity( Node ) when is_atom(Node) ->
	case net_adm:ping( Node ) of
	
		pong ->
			true ;
		
		pang ->
			false
			
	end.			





% Miscellaneous functions.


% Tries to return a string adequate to form a simple name (mostly
% alphanumerical with underscores) from specified term.	
% See also: file_utils:convert_to_filename/1.	
generate_basic_name_from(Term) ->
	String = term_to_string(Term),
	fix_characters(String).	
		

% Flushes all the messages still in the mailbox of this process.
flush_pending_messages() ->
	receive
	
		_ ->
			flush_pending_messages()
	
	after 0 ->
		ok
	
	end.



% Displays a numbered checkpoint. 
% Useful for debugging purposes.
checkpoint(Number) ->
	io:format( "----- CHECKPOINT #~B -----~n", [Number] ).
	
	
	
% Returns the version informations of the current Erlang interpreter being used.
get_interpreter_version() ->
	erlang:system_info(otp_release).





% Non-exported helper functions.

fix_characters(String) ->
	lists:reverse( fix_characters(lists:flatten(String),[]) ).

	
fix_characters([],Acc) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters([32|T],Acc) ->
	fix_characters(T,["_"|Acc]);
	
fix_characters([$'|T],Acc) ->
	fix_characters(T,["_"|Acc]);
	
fix_characters([H|T],Acc) ->
	fix_characters(T,[H|Acc]).
	
