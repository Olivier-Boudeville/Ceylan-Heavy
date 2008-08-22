% Unit tests for the basic utils toolbox.
% See the basic_utils.erl tested module.
-module(basic_utils_test).

-export([run/0]).

-define(Tested_module,basic_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	InitialTimestamp = basic_utils:get_timestamp(),
	
	io:format( "   Timestamp is ~s.~n", [ 
		basic_utils:get_textual_timestamp(InitialTimestamp) ] ),
				
	io:format( "   Output with term_toString : ~s, ~s and ~s.~n", 
		[ basic_utils:term_toString(an_atom), basic_utils:term_toString([1,2]), 
			basic_utils:term_toString("A string")	]),
	
	io:format( "   Converting an integer to a string: ~s.~n",
		[ basic_utils:integer_to_string(3245) ] ),		

	basic_utils:checkpoint(1),
	
	basic_utils:start_random_source(),
	
	RandomList = [ basic_utils:get_random_value(5) || _X <- lists:seq(1,15) ],

	basic_utils:stop_random_source(),
	
	basic_utils:checkpoint(2),
	
	io:format( "   Current module being used as random source: ~w.~n",
		[basic_utils:get_random_module_name()] ),
			
	io:format( "   A list of integer random values between 1 and 5 "
		"(both included): ~w.~n", [RandomList] ),
				
	io:format( "   Displaying the duration of this test: ~s.~n", 
		[ basic_utils:get_textual_duration( InitialTimestamp,
			basic_utils:get_timestamp() ) ] ),
	
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

