% Unit tests for the executable_utils toolbox.
% See the executable_utils.erl tested module.
-module(executable_utils_test).

-export([run/0]).

-define(Tested_module,executable_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	
		
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

