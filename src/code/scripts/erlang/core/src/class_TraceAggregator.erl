% Most basic trace aggregator.
% It just collects traces from emitters and store them at once in a file.
-module(class_TraceAggregator).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).



% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,TraceFilename,IsPrivate).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).


% Method declarations.
-define(wooper_method_export,send/10).

% Static method declarations (to be directly called from module):
-export([create/1,getAggregator/0,remove/0]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% For registration name:
-include("class_TraceAggregator.hrl").

-define(LogPrefix,"[Trace Aggregator]").

% Use global:registered_names() to check aggregator presence.


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).


% Implementation Notes.
%
% The aggregator could store per-emitter constant settings (ex: emitter name
% and categorization) instead of having them sent to it each time.
%


% Constructs a new trace aggregator.
% TraceFilename is the name of the file where traces should be written to.
% IsPrivate tells whether this trace aggregator will be privately held (hence
% should not be registered in naming service) or if it is a (registered) 
% singleton.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes (none), then this class-specific actions:

	PrivateState = case IsPrivate of 
	
		true ->
			io:format( "~s Creating a private trace aggregator, "
				"whose PID is ~w.~n", [ ?LogPrefix, self() ] ),
			?setAttribute(State,is_private,true);
		
		false ->
			io:format( "~s Creating the trace aggregator, whose PID is ~w.~n", 
				[ ?LogPrefix, self() ] ),
	
			case global:register_name( ?trace_aggregator_name, self() ) of 
	
				yes ->
					io:format( "~s Trace aggregator registered globally.~n", 
						[ ?LogPrefix ] );
		
				no ->
					error_logger:info_msg( 
						"~s Trace aggregator could not be registered.~n",
						[ ?LogPrefix ] ),
					exit( trace_aggregator_could_not_register )
			
			end,

			% Registers as well locally:
			true = register( ?trace_aggregator_name, self() ),
			?setAttribute(State,is_private,false)

	end,
	
	{ok,File} = file:open( TraceFilename, write ),

	EndState = ?setAttributes( PrivateState, [ {trace_filename,TraceFilename}, 
		{trace_file,File} ] ),
	io:format( "~s Trace aggregator created.~n", [ ?LogPrefix ] ),
	EndState.

	
	
% Overriden destructor.
delete(State) ->
	io:format( "~s Deleting trace aggregator.~n", [ ?LogPrefix ] ),

	% Class-specific actions:
	case ?getAttr(is_private) of
	
		true ->
			ok;
			
		false ->	 
			unregister( ?trace_aggregator_name ),
			global:unregister_name( ?trace_aggregator_name )
			
	end,
	
	file:close( ?getAttr(trace_file) ),

	% Then call the direct mother class counterparts: (none)
	
	io:format( "~s Trace aggregator deleted.~n", [ ?LogPrefix ] ),
	
	% Allow chaining:
	State.
	
	
	

	
	
% Methods section.


% Sends a full trace to this aggregator to have it stored.
% The nine fields correspond to the ones defined in our trace format.
% (oneway)
send( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ) ->
	io:format( ?getAttr(trace_file), "~w|~s|~s|~w|~s|~s|~s|~w|~s~n", 
		[ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message  ] ),
	?wooper_return_state_only( State ).



% 'Static' methods (module functions):
	
	
% Creates the trace aggregator asynchronously, with default settings.
% (static)	
create(false) ->
	% Trace filename, isPrivate:
	new( ?trace_aggregator_filename, false );

% Creates the trace aggregator synchronously, with default settings.
% (static)	
create(true) ->
	% Trace filename, isPrivate:
	synchronous_new( ?trace_aggregator_filename, false ).



% Returns the Pid of the current trace aggregator.
% Creates it, if it is not already existing.
% Note: to avoid race conditions between concurrent calls to this static 
% method (ex: due to trace emitter instances), a simulation might start with
% a call to this method with a blocking wait until the aggregator pops up in
% registry services.
% Waits a bit before giving up: useful when client and aggregator processes are
% launched almost simultaneously.
% (static)
getAggregator() ->
	% If launching multiple trace emitters in a row, first emitter may 
	% trigger the launch of trace aggregator, but second emitter might do the
	% same if the aggregator is still being initialized: 
	case utils:wait_for_global_registration_of( ?trace_aggregator_name ) of
	
		AggregatorPid when is_pid(AggregatorPid) ->
			% Already available, return its PID:
			AggregatorPid;
		
		{registration_waiting_timeout,?trace_aggregator_name} ->
			% Not available, launch it synchronously (with default settings):
			create(true),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			case utils:wait_for_global_registration_of( ?trace_aggregator_name )
					of
	
				AggregatorPid when is_pid(AggregatorPid) ->
					AggregatorPid;	

				{registration_waiting_timeout,?trace_aggregator_name} ->
					error_logger:error_msg( 
						"class_TraceAggregator:getAggregator "
						"unable to launch successfully the aggregator.~n" ),
					trace_aggregator_launch_failed
						
			end		
			
	end.		
	


% Deletes the trace aggregator.
% (static)	
remove() ->

	case global:whereis_name( ?trace_aggregator_name ) of
	
		undefined ->
			trace_aggregator_not_found;
			
		TraceAggregatorPid ->
			TraceAggregatorPid ! delete
			
	end.


% Section

% Section for helper functions (not methods).


