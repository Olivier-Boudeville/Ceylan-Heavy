% Trace supervisor.
% This version just uses LogMX (http://logmx.com) to track the default 
% simulation trace file, expected to be locally available on disk.
-module(class_TraceSupervisor).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).



% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters, TraceFilename, MonitorNow, Synchronous ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3, 
	synchronous_new/3, synchronous_new_link/3,
	synchronous_timed_new/3, synchronous_timed_new_link/3,
	remote_new/4, remote_new_link/4, remote_synchronous_new/4,
	remote_synchronous_new_link/4, remote_synchronous_timed_new/4,
	remote_synchronous_timed_new_link/4, construct/4, delete/1 ).


% Method declarations.
-define(wooper_method_export, monitor/1, synchronous_monitor/1 ).


% Static method declarations (to be directly called from module):
-export([ create/0, create/1, create/2, create/3 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For default trace filename:
-include("class_TraceAggregator.hrl").

-define(LogPrefix,"[Trace Supervisor]").

% Use global:registered_names() to check supervisor presence.


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).



% Constructs a new trace supervisor.
%  - TraceFilename is the name of the file where traces should be read from.
%  - MonitorNow tells whether the supervision should being immediately (if true)
% or only when the monitor method is called (if false).
%  - Synchronous tells whether the monitoring should be non-blocking
% (if false) or otherwise the monitoring should be blocking and Synchronous
% should be the PID of the caller to be notified.
% This parameter has a meaning iff MonitorNow is true.
construct(State,?wooper_construct_parameters) ->

	io:format( "~s Creating a trace supervisor whose PID is ~w.~n", 
		[ ?LogPrefix, self() ] ),

	% First the direct mother classes (none), then this class-specific actions:
	NewState = ?setAttributes( State, [ {trace_filename,TraceFilename} ] ),
		
	EndState = case MonitorNow of
	
		true ->
			case Synchronous of
			
				Pid when is_pid(Pid) ->
					% Pattern-match the result of in-place invocation:
					% ('monitor_ok' used to be temporarily replaced by '_'
					% due to the LogMX issue with
					% java_security_PrivilegedAction)
					{RequestState,monitor_ok} = executeRequest(
						NewState, synchronous_monitor ),
						 
					% Sends back to the caller:	 
					Pid ! {wooper_result,monitor_ok},
					RequestState;
						 
				none ->
					% Non-blocking, handled after the constructor:		 
					self() ! monitor,
					NewState
			
			end;
		
		false ->
			NewState
			
	end,	
	io:format( "~s Trace supervisor created.~n", [ ?LogPrefix ] ),
	EndState.

	
	
% Overridden destructor.
delete(State) ->
	io:format( "~s Deleting trace supervisor.~n", [ ?LogPrefix ] ),
	% Class-specific actions:
	% Then call the direct mother class counterparts: (none)
	io:format( "~s Trace supervisor deleted.~n", [ ?LogPrefix ] ),
	% Allow chaining:
	State.
	

	
	
% Methods section.


% Triggers an asynchronous supervision (trace monitoring).
% Will return immediately.
% (oneway)
monitor(State) ->
	Filename = ?getAttr( trace_filename ),
	case filelib:is_file( Filename ) of
	
		true ->
			ok;
			
		false ->
			error_logger:error_msg( "class_TraceSupervisor:monitor "
				"unable to find trace file '~s'.~n", [ Filename ] ),
			trace_file_not_found
			
	end,
	io:format( "~s Trace supervisor will monitor file '~s' with LogMX now.~n", 
		[ ?LogPrefix, Filename ] ),
	
	% Non-blocking (logmx.sh must be found in the PATH):
	[] = os:cmd( "logmx.sh " ++ Filename ++ " &" ),
	
	?wooper_return_state_only(State).



% Triggers a synchronous supervision (trace monitoring).
% Will block until LogMX is stopped by the user.
% (request)
synchronous_monitor(State) ->
	Filename = ?getAttr( trace_filename ),
	case filelib:is_file( Filename ) of
	
		true ->
			ok;
			
		false ->
			error_logger:error_msg( "class_TraceSupervisor:synchronous_monitor "
				"unable to find trace file '~s'.~n", [ Filename ] ),
			trace_file_not_found
			
	end,
	io:format( "~s Trace supervisor will monitor file '~s' with LogMX now, "
		"blocking until the user closes the LogMX window.~n", 
		[ ?LogPrefix, Filename ] ),
	
	% Blocking:
	case os:cmd( "logmx.sh " ++ Filename ) of
	
		[] ->
			io:format( "~s Trace supervisor ended monitoring of '~s' "
				"with LogMX.~n", [ ?LogPrefix, Filename ] ),
			?wooper_return_state_result(State,monitor_ok);
	
		Other ->
			error_logger:error_msg(
				"The monitoring of trace supervisor failed: ~s.~n", 
				[ Other ] ),
			?wooper_return_state_result(State,monitor_failed)
			%throw( trace_supervision_failed )
	
	end.





% 'Static' methods (module functions):
	

% Creates the trace supervisor synchronously with default settings regarding
% trace filename and start (immediate here, not deferred).
% See create/3 for a more in-depth explanation of the parameters.
% (static)	
create() ->
	% First parameter is synchronous, second is MonitorNow (immediate):
	create( true, true, ?trace_aggregator_filename ).
	
	
% Creates the trace supervisor, synchronously iff Synchronous is true, with
% default settings regarding trace filename and start (immediate here, not
% deferred).
% See create/3 for a more in-depth explanation of the parameters.
% (static)	
create(Synchronous) ->
	% Second parameter is MonitorNow (immediate):
	create( Synchronous, true, ?trace_aggregator_filename ).


% Creates the trace supervisor, synchronously iff Synchronous is true, with
% default settings regarding start (immediate here, not deferred).
% See create/3 for a more in-depth explanation of the parameters.
% (static)	
create(Synchronous,TraceFilename) ->
	% Second parameter is MonitorNow (immediate):
	create( Synchronous, true, TraceFilename ).


% Creates a trace supervisor.
%  - Synchronous tells whether the monitoring should be blocking (if true) or
% not (the supervisor tool is then launched in the background). 
%  - MonitorNow tells whether the monitoring should start immediately or only
% when a monitor/synchronous_monitor method is called
%  - TraceFilename the trace file to monitor
create(Synchronous,MonitorNow,TraceFilename) ->
	case Synchronous of 
	
		true ->
			new( TraceFilename, MonitorNow, self() ) ;
			
		false ->
			new( TraceFilename, MonitorNow, none )
			
	end.
		


% Section for helper functions (not methods).

