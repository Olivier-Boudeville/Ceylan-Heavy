% Shared constructs for most tests.
% Takes care of 
% Avoids code duplication.


% testFailed exported to avoid a warning if not used.
-export([run/0,testFailed/1]).


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



% Per-test trace file (must be defined before TraceSupervisor include):
-define(TraceFilename,
	io_lib:format( "~s.log", [ ?MODULE ]) 
).


% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").	




-define( test_fatal(Message),
	class_TraceEmitter:send_from_test(fatal,[Message])
).


-define( test_error(Message),
	class_TraceEmitter:send_from_test(error,[Message])
).
	
	
-define( test_warning(Message),
	class_TraceEmitter:send_from_test(warning,[Message])
).


-define( test_info(Message),
	class_TraceEmitter:send_from_test(info,[Message])
).


-define( test_trace(Message),
	class_TraceEmitter:send_from_test(trace,[Message])
).


-define( test_debug(Message),
	class_TraceEmitter:send_from_test(debug,[Message])
).



-define(test_start, 
	% Create first, synchronously (to avoid race conditions), a trace
	% aggregator (false is to specify a non-private i.e. global aggregator).
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be created at start-up).
	class_TraceAggregator:synchronous_new_link( ?TraceFilename, false ),
	?test_info([ io_lib:format( "Testing module(s) ~w.", 
		[ ?Tested_modules ] ) ]),
	?init_trace_supervisor
).



% Displays and flushes all remaining WOOPER results.
check_pending_wooper_results() ->
	receive
	
		{wooper_result,AResult} ->
			?test_info([ io_lib:format( 
				"Following WOOPER result was unread: ~w.~n", AResult ) ]),
			check_pending_wooper_results()
					
	after 
		
		0 ->
			ok
			
	end.
	


-define(test_stop, 
	?test_info([ io_lib:format( "End of test for module(s) ~w.", 
		[ ?Tested_modules ] ) ]),
	?wait_for_any_trace_supervisor,
	check_pending_wooper_results(),
	testFinished()
).


% Allows to defined whether the probe report should be displayed to the user
% after generation.
-define(generateReportForProbe(ProbePid),

	% Avoids adding a bound variable:
	case init:get_argument('-batch') of
	
		{ok,_} ->
			% Boolean means 'display wanted':
			ProbePid ! {generateReport,false,self()};

		_ ->
			ProbePid ! {generateReport,true,self()}
			
	end,
				
	receive
	
		{wooper_result,report_generated} ->
			?test_info([ "Report correctly generated." ])
			
	end

).


% Handle a test failure.
testFailed(Reason) ->
	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable list.
	Message = io_lib:format( "Test failed for module(s) ~w, reason: ~s~n",
		[ ?Tested_modules, Reason ] ), 
	error_logger:error_msg( Message ),
	?test_fatal([ Message ]),
	% Needed, otherwise error_logger will not display anything:	
	timer:sleep(100),	
	erlang:error( "Test ~s failed.", [ ?MODULE ]).
		
