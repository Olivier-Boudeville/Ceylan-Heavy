#!/usr/bin/env escript


% Erlang script example.


main([String]) ->
	io:format( "Hello: ~p", [String] );
	
main(_) ->
	usage().

        
usage() ->
    io:format("Usage: foo bar\n"),
    halt(1).

