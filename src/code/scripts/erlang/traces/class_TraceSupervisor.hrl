% Note: the test_info macro is expected to be defined prior to the
% class_TraceSupervisor.hrl inclusion (ex: included from test_constructs.hrl).
	

% Allows to use an interactive trace supervisor (LogMX) if true, otherwise
% a silent execution.


% Defines trace filename, if not already specified:
-ifndef(TraceFilename).
	-define(TraceFilename,"Ceylan-test.log").
-endif.


% Use the --batch option (ex: erl --batch) to disable the
% use of the trace supervisor:
-define(init_trace_supervisor,
	% By default (with no specific option) a synchronous supervisor is wanted
	% (wait for its launch to complete):
	
	% One '-' already eaten:
	case init:get_argument('-batch') of
		{ok,_} ->
			% Option specified to disable the supervisor:
			no_trace_supervisor_wanted;
			
		_ ->
			% Default: a trace supervisor is used.
			class_TraceSupervisor:create( true, ?TraceFilename ),
			io:format( "Waiting for trace supervisor to be closed." )			
	end			
).


-define(wait_for_any_trace_supervisor,
	case init:get_argument('-batch') of
		{ok,_} ->
			% No supervisor was launched.
			% Let live the system for some time instead:
			timer:sleep(1000);
		
		_ ->
			% A supervisor must be waited for:
			receive

				{wooper_result,monitor_ok} ->
					?test_info([ "Traces successfully monitored." ])

			end
	end
).

