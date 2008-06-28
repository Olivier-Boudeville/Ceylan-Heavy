% Unit tests for the utils toolbox.
% See the utils.erl tested module.

-module(utils_test).

-export([run/0]).

-define(Tested_module,utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	InitialTimestamp = utils:get_timestamp(),
	
	io:format( "   Timestamp is ~s.~n", [ 
		utils:get_textual_timestamp(InitialTimestamp) ] ),
	
	io:format( "   An image file would be ~s.~n", 
		[ utils:get_image_file_gif( "MyImage" ) ] ),	
			
	io:format( "   Output with term_toString : ~s, ~s and ~s.~n", 
		[ utils:term_toString(an_atom), utils:term_toString([1,2]), 
			utils:term_toString("A string")	]),
	
	Separator = ", ",
	ToJoin = [ "January", "February", "March", "April", "May" ],
	
	io:format( "   Joining list ~w with separator '~s', result is : '~s'.~n",
		[ToJoin,Separator,utils:join(Separator,ToJoin)] ),		

	io:format( "   Converting an integer to a string: ~s.~n",
		[ utils:integer_to_string(3245) ] ),		

	io:format( "   Displaying the duration of this test: ~s.~n", 
		[ utils:get_textual_duration( InitialTimestamp,
			utils:get_timestamp() ) ] ),
		
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().
