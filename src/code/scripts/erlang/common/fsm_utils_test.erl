% Unit tests for the fsm_utils toolbox.
% See the fsm_utils.erl tested module.

-module(fsm_utils_test).

-export([run/0]).

-define(Tested_module,fsm_utils).

% For FSM macro defines:
-include("fsm_utils.hrl").


run() ->
	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	StartFsmState = fsm_utils:create_blank_fsm_state(),
	FsmState = ?setFsmAttribute(StartFsmState, test_question, 42),
	42 = ?getFsmAttr( test_question ),

	FsmStateTwo = fsm_utils:setFsmAttribute(
		FsmState, other_test_question, 43),
	43 = fsm_utils:getFsmAttribute( FsmStateTwo, other_test_question ),
	
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().
