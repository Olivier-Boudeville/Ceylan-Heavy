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



send_traces( 0 ) ->
	ok;
	
send_traces( Count ) ->
	?test_trace([ io_lib:format( "Emitting trace #~s.", 
		[utils:integer_to_string(Count)] ) ]),
	send_traces( Count-1 ).

	
send_timed_traces( 0 ) ->
	ok;
	
send_timed_traces( Count ) ->
	?test_trace([ io_lib:format( "Emitting timed trace #~s.", 
		[utils:integer_to_string(Count)] ) ]),
	timer:sleep(500),	
	send_timed_traces( Count-1 ).


% Run the tests.
run() ->

	io:format( ?Prefix "Testing module ~w. "
		"'make traceManagement_run' supposed to be already executed.~n", 
		[ ?Tested_modules ] ),
	
	
	case init:get_argument('-batch') of
	
		{ok,_} ->
			io:format( ?Prefix "Running in batch mode.~n" );

		_ ->
			io:format( ?Prefix "Running in interactive mode.~n" )
			
	end,
	
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



	% No ?test_start: we will be using the aggregator from then node
	% named 'traceManagement_run'.
	case init:get_argument('-batch') of
		{ok,_} ->
			% Option specified to disable the supervisor:
			no_trace_supervisor_wanted;
			
			
		_ ->
			io:format( ?Prefix "Creating a test trace listener.~n" ),
			MyTraceListener =
				class_TraceListener:synchronous_new_link(AggregatorPid),

			send_timed_traces( 20 ),

			io:format( ?Prefix "Deleting this test trace listener.~n" ),
	
			MyTraceListener ! delete

			
	end,			
			

	?test_stop.

