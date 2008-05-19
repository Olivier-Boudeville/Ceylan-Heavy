% Unit tests for the implementation of trace management.
% @see the class_TraceEmitter.erl module.
% @see the class_TraceAggregator.erl module.
% @see the class_TraceSupervisor.erl module.
% Note: trace services are among the most generic services offered, they are
% used in the vast majority of tests but this one, as the purpose of this test
% is actually to test traces by themselves (cannot use the trace system to test
% the trace system !). 
-module(traceManagement_test).


-define(Tested_modules,[ class_TraceEmitter, class_TraceAggregator,
	class_TraceSupervisor ]).


% For all facilities common to all tests:
-include("test_constructs.hrl").



-define(Prefix,"--> ").



% Run the tests.
% Note: this is the only test that does not use the trace functionalities
% for its own behaviours (since it is the subject of the test).
run() ->

	io:format( ?Prefix "Testing module ~w.~n", [ ?Tested_modules ] ),
	
	io:format( ?Prefix "Starting Trace system, with a trace aggregator "
		"and, if requested, a trace supervisor.~n" ),
	?test_start,
	
	
	case init:get_argument('-batch') of
	
		{ok,_} ->
			io:format( ?Prefix "Running in batch mode.~n" );

		_ ->
			io:format( ?Prefix "Running in interactive mode.~n" )
			
	end,
	
	
	io:format( ?Prefix "Creating a new TestTraceEmitter.~n" ),
	
	Name = "I am a test emitter of traces",
	
	% Should not trigger the launch of another global aggregator:
	% (as test_start triggers a *synchronous* aggregator):
	MyTraceEmitter = class_TestTraceEmitter:synchronous_new_link(Name),	
		
	?test_fatal([   "This is a test of the fatal priority for tests." ]),
	?test_error([   "This is a test of the error priority for tests." ]),
	?test_warning([ "This is a test of the warning priority for tests." ]),
	?test_info([    "This is a test of the info priority for tests." ]),
	?test_trace([   "This is a test of the trace priority for tests." ]),
	?test_debug([   "This is a test of the debug priority for tests." ]),
	
	io:format( ?Prefix 
		"Requesting the TestTraceEmitter to send some traces.~n" ),
	
	% Wait until there is an answer for this trace emitter:
	MyTraceEmitter ! {sendTraces,[],self()},
	
	receive
	
		{wooper_result,ok} ->
			io:format( ?Prefix "Traces sent.~n" )	
						
	end,

	MyTraceEmitter ! {getName,[],self()},
	receive
	
		{wooper_result,Name} ->
			?test_info([ "Correct name returned." ])
			
	end,
	
	NewName = "This is my new name",
	
	MyTraceEmitter ! {setName,[NewName]},

	MyTraceEmitter ! {getName,[],self()},
	receive
	
		{wooper_result,NewName} ->
			?test_info([ "Correct new name returned." ])
			
	end,
	
	
	io:format( ?Prefix "Deleting this TestTraceEmitter.~n" ),
	
	MyTraceEmitter ! delete,

	?test_stop.

