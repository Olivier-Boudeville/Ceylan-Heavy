% Unit tests for the file_utils toolbox.
% See the file_utils.erl tested module.
-module(file_utils_test).

-export([run/0]).

-define(Tested_module,file_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	
	io:format( "   An image file would be ~s.~n", 
		[ file_utils:get_image_file_gif( "MyImage" ) ] ),	
			
	Separator = ", ",
	ToJoin = [ "January", "February", "March", "April", "May" ],
	
	io:format( "   Joining list ~w with separator '~s', result is : '~s'.~n",
		[ToJoin,Separator,file_utils:join(Separator,ToJoin)] ),		
		
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

