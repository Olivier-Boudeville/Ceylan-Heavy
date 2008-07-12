% Unit tests for the file_utils toolbox.
% See the file_utils.erl tested module.
-module(file_utils_test).

-export([run/0]).

-define(Tested_module,file_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	
		
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

