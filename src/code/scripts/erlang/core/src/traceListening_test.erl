% Unit tests for the implementation of trace listening.
% See the class_TraceListener.erl module.
% See the class_TraceSupervisor.erl module.
% See the class_TraceAggregator.erl module.
% Made to be executed while the traceManagement_test is running.
-module(traceListening_test).


-define(Tested_modules,[ class_TraceListener, class_TraceAggregator,
	class_TraceSupervisor ]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


-define(Prefix,"--> ").


% ?test_stop should not be used here as its wait_for_any_trace_supervisor 
% macro would wait for a non-launched supervisor.


send_traces( 0 ) ->
	ok;
	
send_traces( Count ) ->
	?test_trace([ io_lib:format( "Emitting trace #~B.", [Count] ) ]),
	send_traces( Count-1 ).

	
send_timed_traces( 0 ) ->
	ok;
	
send_timed_traces( Count ) ->
	?test_trace([ io_lib:format( "Emitting timed trace #~B.", [Count] ) ]),
	timer:sleep(100),	
	send_timed_traces( Count-1 ).



% The real code of the test, in a separate function to avoid an indentation
% offset.
test_actual_body() ->

	[_H,NodeName] = string:tokens( atom_to_list(node()), "@" ),
	
	TargetVMName = lists:flatten( 
		io_lib:format( "traceManagement_run@~s", [NodeName] ) ),
		
	io:format( ?Prefix "Connecting to '~s'.~n", [TargetVMName]),
	
	pong = net_adm:ping( list_to_atom(TargetVMName) ),
	
	% Otherwise the remote node could not be known before use:
	global:sync(),
	
	io:format( ?Prefix "Globally registered names: ~w.~n",
		[global:registered_names()]),
	
	AggregatorName = ?trace_aggregator_name ,
	io:format( ?Prefix "Looking up aggregator by name: ~s.~n",
		[AggregatorName] ),
		
	AggregatorPid = case global:whereis_name( AggregatorName ) of
	
		Pid when is_pid(Pid) ->
			Pid
	
	end,
	
	io:format( ?Prefix "Sending initial traces to force "
		"a real synchronization.~n" ),
	send_traces( 50 ),

	% No ?test_start: we will be using the aggregator from the node
	% named 'traceManagement_run'.
	io:format( ?Prefix "Creating a test trace local listener.~n" ),
	MyTraceListener = class_TraceListener:synchronous_new_link(AggregatorPid),

	send_timed_traces( 20 ),

	io:format( ?Prefix "Deleting this test trace listener.~n" ),
	
	MyTraceListener ! delete,
			
	% To ensure the message has been sent before the VM shuts down:
	timer:sleep(500),
			
	?test_info([ io_lib:format( "End of test for module(s) ~w.", 
		[ ?Tested_modules ] ) ]),
	check_pending_wooper_results(),
	testFinished().



% Run the tests.
run() ->

	io:format( ?Prefix "Testing module ~w. "
		"'make traceManagement_run' supposed to be already executed.~n", 
		[ ?Tested_modules ] ),
	
	
	case init:get_argument('-batch') of
	
		{ok,_} ->
			io:format( ?Prefix "Running in batch mode, no traceManagement_test "
				"supposed to be running, nothing done.~n" ),
			io_lib:format( "End of test for module(s) ~w.",
				[ ?Tested_modules ] ),
			check_pending_wooper_results(),
			testFinished();
					
		_ ->
			io:format( ?Prefix "Running in interactive mode, "
				"'make traceManagement_run' supposed to be already running.~n"
			),
			test_actual_body()
			
	end.
	
