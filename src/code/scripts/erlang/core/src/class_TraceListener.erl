% Trace listener, similar to a remote supervisor.
% This version just uses LogMX (http://logmx.com) to track the default 
% simulation trace file, which will be synchronized automatically: 
% history will be retrieved under a zipped form from the aggregator, and next
% traces will be sent directly to this listener as well as to the aggregator.
-module(class_TraceListener).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).



% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,TraceAggregatorPid).

% Life-cycle related exported operators:
-define(wooper_construct_export, new/1, new_link/1, 
	synchronous_new/1, synchronous_new_link/1, construct/2, delete/1).


% Method declarations.
-define(wooper_method_export, monitor/1, addTrace/2).

% Static method declarations (to be directly called from module):
-export([create/1]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-define(LogPrefix,"[Trace Listener]").


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).


% Implementation Notes.
%


% Constructs a new trace listener.
% TraceAggregatorPid is the PID of the trace aggregator to which this listener
% will be synchronized.
construct(State,?wooper_construct_parameters) ->

	io:format( "~s Creating a trace listener whose PID is ~w, "
		"synchronized on trace aggregator ~w.~n", 
		[ ?LogPrefix, self(), TraceAggregatorPid ] ),

	% First the direct mother classes (none), then this class-specific actions:
	
	io:format( "~s Requesting from aggregator a trace synchronization.~n", 
		[ ?LogPrefix ] ),
		
	TraceAggregatorPid ! {addTraceListener,self()},
	receive
	
		 {trace_zip,Bin,TraceFilename} ->
		 	% Allows to run for the same directory as aggregator:
			ListenerTraceFilename = "Listener-" ++ TraceFilename,
			io:format( "~s Received from aggregator a trace synchronization "
				"for file '~s', will store it in '~s'.~n", 
				[ ?LogPrefix, TraceFilename, ListenerTraceFilename] ),
			file_utils:zipped_term_to_unzipped_file(Bin,ListenerTraceFilename)
				
	end,
		
	% Will write in it newer traces:
	{ok,File} = file:open( ListenerTraceFilename, [append] ),
		
	NewState = ?setAttributes( State, [ 
			{trace_aggregator_pid,TraceAggregatorPid},
			{trace_filename,ListenerTraceFilename},
			{trace_file,File}
	] ),
	
	{wooper_result,EndState} = executeOneway(NewState,monitor),
	
	io:format( "~s Trace listener created.~n", [ ?LogPrefix ] ),
	EndState.

	
	
% Overriden destructor.
delete(State) ->
	io:format( "~s Deleting trace listener.~n", [ ?LogPrefix ] ),

	% Class-specific actions:
	?getAttr(trace_aggregator_pid) ! {removeTraceListener,self()},
	file:close( ?getAttr(trace_file) ),
	% Then call the direct mother class counterparts: (none)
	io:format( "~s Trace listener deleted.~n", [ ?LogPrefix ] ),
	
	% Allow chaining:
	State.
	

	
	
% Methods section.


% Triggers an asynchronous supervision (trace monitoring).
% Will return immediately.
% Note: directly inspired from class_TraceSupervisor.erl.
% (oneway)
monitor(State) ->
	Filename = ?getAttr( trace_filename ),
	case filelib:is_file( Filename ) of
	
		true ->
			ok;
			
		false ->
			error_logger:error_msg( "class_TraceListener:monitor "
				"unable to find trace file '~s'.~n", [ Filename ] ),
			trace_file_not_found
			
	end,
	io:format( "~s Trace listener will monitor file '~s' with LogMX now.~n", 
		[ ?LogPrefix, Filename ] ),
	
	% Non-blocking (logmx.sh must be found in the PATH):
	[] = os:cmd( "logmx.sh " ++ Filename ++ " &" ),
	
	?wooper_return_state_only(State).


% Registers a new pre-formatted trace in trace file.
% To be called by the trace aggregator.
% (oneway)
addTrace(State,NewTrace) ->
	io:format( ?getAttr(trace_file), "~s", [binary_to_list(NewTrace)] ),
	?wooper_return_state_only(State).

	

% 'Static' methods (module functions):
	
	
	
% Creates the trace listener that will synchronize itself to the specified
% aggregator.
% (static)	
create(AggregatorPid) ->
	new( AggregatorPid ).
		


% Section for helper functions (not methods).

