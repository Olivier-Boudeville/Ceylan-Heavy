% Copyright (C) 2003-2010 Olivier Boudeville
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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
% See basic_utils_test.erl for the corresponding test.
-module(basic_utils).



% Note that string:tokens can be used to split strings.



% Timestamp-related functions.
-export([ get_timestamp/0, get_textual_timestamp/0, get_textual_timestamp/1,
	timestamp_to_string/1, get_duration/2, get_textual_duration/2 ]).
	
	
	
% String management functions.
-export([ term_toString/1, term_to_string/1, integer_to_string/1,
	string_list_to_string/1, ipv4_to_string/1, ipv4_to_string/2, 
	version_to_string/1, join/2,
	remove_ending_carriage_return/1, format_text_for_width/2, pad_string/2,
	is_string/1, get_whitespaces_list/0 ]).

	
	
% Registration functions.
-export([ register_as/2, register_as/3, unregister/2, 
	get_registered_pid_for/1, get_registered_pid_for/2,
	wait_for_global_registration_of/1, wait_for_local_registration_of/1 ]).



% Random-related functions.
-export([ start_random_source/3, start_random_source/1, stop_random_source/0,
	get_random_value/0, get_random_value/1, get_random_module_name/0,
	get_random_seed/0, random_select/2, random_permute/1 ]).
	
	

% List management functions.
-export([ get_element_at/2, remove_element_at/2, subtract_all_duplicates/2 ]). 



% User-related functions.
-export([ speak/1, notify_user/1, notify_user/2 ]).



% Node-related functions.
-export([ generate_valid_node_name_from/1, check_node_validity/1 ]). 



% Miscellaneous functions.
-export([ generate_basic_name_from/1, flush_pending_messages/0, checkpoint/1,
		  get_interpreter_version/0, compare_versions/2 ]).





% Timestamp-related functions.


% Returns a tuple describing the current time. 
% Ex: {{2007,9,6},{15,9,14}}
get_timestamp() ->
	% Was: {erlang:date(),erlang:time()}.
	% Better:
	erlang:localtime().
	
		

% Returns a string corresponding to the current timestamp.
get_textual_timestamp() ->
	get_textual_timestamp( get_timestamp() ).
	
	
	
get_textual_timestamp({{Year,Month,Day},{Hour,Minute,Second}}) ->
 	io_lib:format( "~p/~p/~p ~B:~2..0B:~2..0B",
		[Year,Month,Day,Hour,Minute,Second] ).



% Alias of get_textual_timestamp.
timestamp_to_string(Timestamp) ->	
	get_textual_timestamp(Timestamp).
	
	
	
% Returns the (signed) duration in seconds between the two specified timestamps,
% using the first one as starting time and the second one as stopping time.	
get_duration(FirstTimestamp,SecondTimestamp) ->
	First  = calendar:datetime_to_gregorian_seconds(FirstTimestamp),
	Second = calendar:datetime_to_gregorian_seconds(SecondTimestamp),
	Second - First.


% Returns a textual description of the duration between the two specified
% timestamps.	
get_textual_duration(FirstTimestamp,SecondTimestamp) ->
	{Days,{Hour, Minute, Second}} = calendar:seconds_to_daystime( 
		get_duration(FirstTimestamp,SecondTimestamp) ),

	case Days of 
	
		0 ->
			
			case Hour of
			
			
				0 ->
				
					case Minute of 
					
						0 ->
							lists:flatten( io_lib:format( "~B second(s)", 
								[Second] ) );
						
						_NonZeroMinutess ->
							lists:flatten( io_lib:format( "~B minute(s) "
								"and ~B second(s)", [Minute, Second] ) )
					end;	
						
				_NonZeroHours ->
					lists:flatten( io_lib:format( "~B hour(s), ~B minute(s) "
						"and ~B second(s)", [Hour, Minute, Second] ) )
			
			end;
			
		_NonZeroDays ->
			lists:flatten( io_lib:format( "~B day(s), ~B hour(s), ~B minute(s) "
				"and ~B second(s)", [Days, Hour, Minute, Second] ) )
		
	end.	

		
		
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
% Useless when using functions like io:format, that accept iolists as 
% parameters.	
integer_to_string(IntegerValue) ->
	hd( io_lib:format( "~B", [IntegerValue] ) ).
	


% Returns a string which pretty-prints specified list of strings, with bullets.	
string_list_to_string( ListOfStrings ) ->
	io_lib:format( "~n~s", [ string_list_to_string( ListOfStrings, [] ) ] ).


string_list_to_string( [], Acc ) ->
	 Acc;	

string_list_to_string( [H|T], Acc ) when is_list(H) ->
	string_list_to_string( T, Acc ++ io_lib:format( " + ~s~n", [H] ) ).
	
		
		
% Returns a string describing the specified IPv4 address.
ipv4_to_string( {N1,N2,N3,N4} ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B", [N1,N2,N3,N4] ) ).
	
	
ipv4_to_string( {N1,N2,N3,N4}, Port ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B:~B", [N1,N2,N3,N4,Port] ) ).



% Returns a string describing the specified three-element version.
version_to_string( {V1,V2,V3} ) ->
	io_lib:format( "~B.~B.~B", [V1,V2,V3] ).
	 
	

% join(Separator,ListToJoin), ex: join( '-', [ "Barbara", "Ann" ] ).
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation. 
% Inspired from http://www.trapexit.org/String_join_with.
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
% Note: use string:tokens to split the string.
join( _Separator, [] ) ->
    "";

join( Separator, ListToJoin ) ->
    lists:flatten( lists:reverse( join(Separator, ListToJoin, []) ) ).
	
	
join( _Separator, [], Acc ) ->
    Acc;

join( _Separator, [ H | [] ], Acc ) ->
    [H|Acc];
	
join( Separator, [H|T], Acc ) ->
    join( Separator, T, [ Separator, H | Acc ] ).



% Removes the ending "\n" character(s) of specified string.
remove_ending_carriage_return(String) when is_list(String) ->
	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).




% Formats specified text according to specified width, expressed in characters.
% Returns a line of strings, each of which having Width characters.
format_text_for_width( Text, Width ) ->
	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten(Text), "\\s+", " ", 
		[global,{return, list}] ), 
	WordList = string:tokens( CleanedTest, " " ),
	%io:format( "Formatting ~p.~n", [WordList] ),
	join_words( WordList, Width ).
	
	

% Joins words from the list, line by line.
join_words( WordList, Width ) ->
	join_words( WordList, Width, _Lines = [], _CurrentLine = "",
		_CurrentLineLen = 0 ).
	
	
join_words( [], _Width, AccLines, _CurrentLine, _CurrentLineLen = 0 ) ->
	% Ended with a full line:
	lists:reverse(AccLines);
		
join_words( [], Width, AccLines, CurrentLine, _CurrentLineLen ) ->
	% Ended with a partial line:
	lists:reverse( [pad_string(CurrentLine,Width)|AccLines] );
		
join_words( [Word|RemainingWords], Width, AccLines, CurrentLine,
		CurrentLineLen ) ->
		
	%io:format( "Managing word '~s' (len=~B), current line is '~s' (len=~B), "
	%	"width = ~B.~n", [Word,length(Word),CurrentLine,CurrentLineLen,Width] ),
			
	% Length should be incremented, as a space must be inserted before that
	% word, however we want to accept words whose width would be exactly equal
	% to the line width:
	ActualLength = case CurrentLine of 
	
		"" ->
			length(Word);
			
		_NonEmpty ->
			% Already at least a letter, we therefore must add a space before
			% the new word:
			length(Word) + 1
				
	end,
	case ActualLength of 
	
		CompatibleWidth when CompatibleWidth =< Width ->
			% Word width is manageable.
			% Will this word fit on the current line?
			% It depends on the
			case CurrentLineLen + CompatibleWidth of
			
				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					{NewCurrentLine,NewLineLen} = case CurrentLineLen of 
					
						0 ->
							{Word,CompatibleWidth};
							
						Len -> 
							{CurrentLine ++ " " ++ Word,Len+CompatibleWidth+1}	
					
					end,		
					%io:format("Current line is now '~s'.~n", [NewCurrentLine]),
					join_words( RemainingWords, Width, AccLines, NewCurrentLine,
						NewLineLen );
				
				_ExceedingLen ->
					% No, with this word the current line would be too wide,
					% inserting it on new line instead:
					PaddedCurrentLine = pad_string( CurrentLine, Width ),
					%io:format( "Inserting line '~s'.~n", [PaddedCurrentLine] ),
					join_words( RemainingWords, Width,
						[PaddedCurrentLine|AccLines], Word, CompatibleWidth )
			
			end;
			
		_TooLargeWidth ->
			% Will break words as many times as needed:
			%io:format( "Word '~s' is too large (len=~B), breaking it.~n", 
			%	[Word,length(Word)] ),
			Subwords = break_word(Word,Width),
			PaddedCurrentLine = pad_string( CurrentLine, Width ),
			join_words( Subwords ++ RemainingWords, Width,
				[PaddedCurrentLine|AccLines], "", 0 )
			
	end.



% Returns the specified string, padded with spaces to specified width,
% left-justified.
pad_string( String, Width ) when length(String) =< Width ->
	lists:flatten( io_lib:format( "~*.s", [ -Width, String ] ) ).



% Returns true iff the parameter is a string.
% Taken from http://lethain.com
% (see distinguishing-strings-from-lists-in-erlang)
% Note: something like [ $e, 1, 2, $r ] is deemed to be a string.
is_string( [] ) -> 
	true;
	
is_string( [H|_] ) when not is_integer(H) -> 
	false;
	
is_string( [_|T] ) -> 
	is_string(T);
	
is_string( _Other ) ->
	false.



% Returns the list of known whitespaces.
% Note: useful with string:tokens.
get_whitespaces_list() ->
	" \n\t\r".
	
	

% Returns a list of words obtained from the breaking of specified word, 
% according to specified maximum width.
% Parts of that word will use a separating dash.
% Ex: break_word("simulator",5) returns ["simu-","lator"]
break_word(Word,Width) ->
	%io:format( "Breaking '~s' for width ~B.~n", [Word,Width] ),
	%timer:sleep(500),
	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	%CutIndex = length(Word) div 2,
	%CutIndex = Width-1,
	cut_into_chunks( Word, Width, [] ).
	
	
	
% Cuts specified string into pieces, each of them having to fit in specified
% width.
cut_into_chunks( _String = [], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [lists:reverse(Acc)]),
	lists:reverse(Acc);

% Last word may take the full width (no dash to add):	
cut_into_chunks( String, ChunkSize, Acc ) when length(String) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [String|Acc] );
	
% Here we have to cut the string anyway:	
cut_into_chunks( String, ChunkSize, Acc ) ->
	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)
	{FirstPart,Remaining} = aggregate_word( String, ChunkSize-1, [] ),
	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~s' (~B), Remaining = '~s'.~n",
	%	[FirstPart,length(FirstPart),Remaining] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" |Acc] ).

		
		
aggregate_word( String, 0, Acc ) ->
	{lists:reverse(Acc),String};
	
	
% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
aggregate_word( String = [$_|_T], 1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:	
aggregate_word( [$_|T], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [$\_,$\\|Acc] );
	
aggregate_word( [H|T], Count, Acc ) ->
	aggregate_word( T, Count-1, [H|Acc] ).
	
	


% Registration functions.
	
	
% Registers the current process under specified name.
% Declaration is register_as(Name,RegistrationType) with 
% RegistrationType in 'local_only', 'global_only', 'local_and_global', 'none'
% depending on what kind of registration is requested.
% Returns ok on success, otherwise throws an exception.
register_as( Name, RegistrationType ) ->
	register_as( self(), Name, RegistrationType ).



% Registers specified PID under specified name.
% Declaration is: register_as(Pid,Name,RegistrationType) with 
% RegistrationType in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested.
% Returns ok on success, otherwise throws an exception.
register_as( Pid, Name, local_only ) ->
	try erlang:register( Name, Pid ) of 
		
		true ->
			ok
			
	catch
	
		ExceptionType:Exception ->			
			throw( {local_registration_failed,Name,{ExceptionType,Exception}} )
				
	end;
 
register_as( Pid, Name, global_only ) ->
	case global:register_name( Name, Pid ) of 
	
		yes ->
			ok;
					
		no ->
			throw( {global_registration_failed,Name} )				
			
	end;

register_as( Pid, Name, local_and_global ) ->
	ok = register_as(Pid,Name,local_only),
	ok = register_as(Pid,Name,global_only);

register_as(_Pid,_Name,none) ->
	ok.




% Unregisters specified name from specified registry.
% Throws an exception in case of failure.
unregister( Name, local_only ) ->
	try erlang:unregister( Name ) of 
	
		true ->
			ok
	
	catch		

		ExceptionType:Exception ->			
			throw( 
				{local_unregistration_failed,Name,{ExceptionType,Exception}} )
				
	end;

unregister( Name, global_only ) ->
	% Documentation says it returns "void" (actually 'ok'):
	try 
		
		global:unregister_name( Name ) 
	
	catch				
			
		ExceptionType:Exception ->			
			throw( 
				{global_unregistration_failed,Name,{ExceptionType,Exception}} )
				
	end;

unregister( Name, local_and_global ) ->
	ok = unregister( Name, local_only ),
	ok = unregister( Name, global_only );

unregister(_Name,none) ->
	ok.



% Returns the Pid that should be already registered, as specified name.
% Local registering will be requested first, if not found global one will
% be tried.
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
get_registered_pid_for( Name ) ->
	get_registered_pid_for( Name, local_otherwise_global ).
	
	
	
get_registered_pid_for( Name, local_otherwise_global ) ->
	try 
	
		get_registered_pid_for( Name, local )

	catch 
	
		{not_registered_locally,_Name} ->
		
			try 
			
				get_registered_pid_for( Name, global )
					
			
			catch
			
				{not_registered_globally,Name} ->
					throw( {neither_registered_locally_nor_globally,Name} )
					
			end
	
	end;
			
get_registered_pid_for( Name, local ) ->
	case erlang:whereis( Name ) of 
	
		undefined ->
			throw( {not_registered_locally,Name} );
				
		Pid ->
			Pid
			
	end;
	
get_registered_pid_for( Name, global ) ->
	case global:whereis_name( Name ) of 
	
		undefined ->
			throw( {not_registered_globally,Name} );
				
		Pid ->
			Pid
			
	end;
	
% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_type defines).
get_registered_pid_for( Name, local_and_global ) ->
	get_registered_pid_for( Name, local_otherwise_global ).
	




% Waits (up to 5 seconds) until specified name is globally registered.
% Returns the resolved Pid, or throws 
% {global_registration_waiting_timeout,Name}.
wait_for_global_registration_of(Name) ->
	wait_for_global_registration_of(Name,5).
	

wait_for_global_registration_of(Name,0) ->
	throw({global_registration_waiting_timeout,Name});
	
wait_for_global_registration_of(Name,SecondsToWait) ->
	case global:whereis_name( Name ) of 
	
		undefined ->
			timer:sleep(1000),
			wait_for_global_registration_of(Name,SecondsToWait-1);
				
		Pid ->
			Pid
			
	end.




% Waits (up to 5 seconds) until specified name is locally registered.
% Returns either the resolved Pid or {,Name}.
% Returns the resolved Pid, or throws 
% {local_registration_waiting_timeout,Name}.
wait_for_local_registration_of(Name) ->
	wait_for_local_registration_of(Name,5).
	

wait_for_local_registration_of(Name,0) ->
	throw({local_registration_waiting_timeout,Name});
	
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
%  - there is no crypto function returning a random float uniformly 
% distributed between 0.0 and 1.0, and it may not be easy to implement it 
% from what is available
%
% Therefore the two modules are not completely interchangeable.
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


% Returns a random float uniformly distributed between 0.0 and 1.0, updating
% the random state in the process dictionary.
get_random_value() ->
	% Not available: crypto:rand_uniform(0.0,1.0).
	not_available_with_crypto.


% Returns the name of the module managing the random generation.	
get_random_module_name() ->
	crypto.
	
	
-else. % use_crypto_module


% Default random module used here.
% The seed and state management is per-process (stored in the process
% dictionary).

start_random_source(A,B,C) ->
	random:seed(A,B,C).


% Seeds the random number generator, either with specified seed, or with 
% a default seed (if wanting to obtain the same random series at each run) 
% or with current time (if wanting "real" non-reproducible randomness).
start_random_source( {A,B,C} ) ->
	start_random_source(A,B,C);
	
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


% Returns a random float uniformly distributed between 0.0 and 1.0, updating
% the random state in the process dictionary.
get_random_value() ->
	random:uniform().
	

% Returns the name of the module managing the random generation.	
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
	% Replaces all series of spaces by one underscore:
	re:replace( lists:flatten(Name), " +", "_", [global,{return, list}] ).



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



% Compares the two triplets, which describes two version numbers (ex: {0,1,0})
% and returns either first_bigger, second_bigger, or equal.
% Note: the default term order is already what we needed.
compare_versions( {A1,A2,A3}, {B1,B2,B3} ) ->
	case {A1,A2,A3} > {B1,B2,B3} of
	
		true ->
			first_bigger;
			
		false ->
		
			case {A1,A2,A3} =:= {B1,B2,B3} of	
			
				true ->
					equal;
					
				false ->
					second_bigger
					
			end		
	end.



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
	

