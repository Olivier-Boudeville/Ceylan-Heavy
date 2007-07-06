-module(ceylan_utils_test).

-export([run/0]).

run() ->
	io:format( "Testing module ~s.~n", [ ?MODULE ] ),
	io:format( "Timestamp is ~s.~n", [ ceylan_utils:get_timestamp() ] ),
	io:format( "An image file would be ~s.~n", 
		[ ceylan_utils:get_image_file_gif( "MyImage" ) ] ),		
	io:format( "Output with term_toString : ~s, ~s and ~s.~n", 
		[ 
			ceylan_utils:term_toString(an_atom), 
			ceylan_utils:term_toString([1,2]), 
			ceylan_utils:term_toString("A string")
		]),
	io:format( "End of test for module ~s.~n", [ ?MODULE ] ).
