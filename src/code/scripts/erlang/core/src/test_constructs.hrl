% Shared constructs for most tests.
% Takes care of 
% Avoids code duplication.


% testFailed exported to avoid a warning if not used.
-export([run/0,wait_ready/0,suspend_simulation_until_enter_pressed/1,
	testFailed/1]).


% Comment out to be able to use the interpreter after the test:
-define(ExitAfterTest,).

-ifdef(ExitAfterTest).

testFinished() ->
	io:format( "(test finished, interpreter halted)~n" ),	
	erlang:halt().
	
-else.

testFinished() ->
	io:format( "(test finished, interpreter still running)~n" ),
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


% Allows to define whether the probe report should be displayed to the user,
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
	
		{wooper_result,probe_report_generated} ->
			?test_info([ "Probe report correctly generated." ])
			
	end

).



% Allows to define whether the topological view should be displayed to the user,
% after generation.
-define(generateTopologicalViewFor(Pid),

	% Avoids adding a bound variable:
	case init:get_argument('-batch') of
	
		{ok,_} ->
			% Boolean means 'display wanted':
			Pid ! {generateTopologicalView,false,self()};

		_ ->
			Pid ! {generateTopologicalView,true,self()}
			
	end,
				
	receive
	
		{wooper_result,topological_view_generated} ->
			?test_info([ "Topological view correctly generated." ])
			
	end

).



% Waits until a model is ready, and acknowledges its notification.
wait_ready() ->
	receive 
	
		{ actorMessage,	[_ATick,notifyReady,ModelPid] } ->
			?test_debug([ io_lib:format("Model ~w ready.",[ModelPid]) ]),
			% Acknowledges the actor message, otherwise model will be frozen:
			ModelPid ! {acknowledgeMessage,self()}
		
	end.


% Suspends the simulation until the Enter key is pressed.
suspend_simulation_until_enter_pressed(TimeManagerPid) ->
	case init:get_argument('-batch') of

		{ok,_} ->
			nothing_done;

		_ ->
			%io:format("Requesting the simulation to be suspended.~n"),
			TimeManagerPid ! suspend,
			io:get_line("Simulation requested to be suspended, "	
				"press Enter to resume it."),
			TimeManagerPid ! resume,
			io:format("Simulation requested to be resumed.~n")
			
	end.
	
	
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
		
