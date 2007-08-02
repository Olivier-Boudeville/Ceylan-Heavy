% Unit tests for the WOOPER class manager implementation.
% See the wooper_class_manager.erl tested module.

-module(wooper_class_manager_test).

-export([run/0]).

-define(Tested_module,wooper_class_manager).

-define(Prefix,"--> ").


% Comment out to be able to use the interpreter after the test:
-define(ExitAfterTest,).

-ifdef(ExitAfterTest).

testFinished() ->
	erlang:halt().
	
-else.

testFinished() ->
	io:format( "(interpreter still running)~n" ),
	test_success.
	
-endif.

% For WooperClassManagerName:
-include("wooper_class_manager.hrl").


run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Spawning module ~s.~n", [ ?Tested_module ] ),
	spawn(?Tested_module,start,[self()]),	
	receive
			
		class_manager_registered ->
			io:format( ?Prefix "Requesting its state display.~n" ),
			?WooperClassManagerName ! display
			
	% 10-second time-out:
	after 10000	->
		erlang:error( "#### Error: wooper_get_class_manager: unable to find "
			"class manager after 10s, test failed." )
				
	end,
	io:format( ?Prefix "Requesting it to stop.~n" ),
	?WooperClassManagerName ! stop,
	% Probably, if ExitAfterTest is set, the test will stop before the
	% manager itself.
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

