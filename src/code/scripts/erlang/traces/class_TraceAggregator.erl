% Most basic trace aggregator.
% It just collects traces from emitters and store them at once in a file.
% Trace listeners can connect to the aggregator. In this case it will stop and 
% send first the full current trace file to them. From that moment, incoming
% traces will be both written in file and sent to each trace listener still
% connected.
-module(class_TraceAggregator).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).



% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters, TraceFilename, IsPrivate ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2, 
	synchronous_new/2, synchronous_new_link/2,
	synchronous_timed_new/2, synchronous_timed_new_link/2,
	remote_new/3, remote_new_link/3, remote_synchronous_new/3,
	remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
	remote_synchronous_timed_new_link/3, construct/3, delete/1 ).



% Method declarations.
-define(wooper_method_export, send/10, addTraceListener/2, 
	removeTraceListener/2 ).


% Static method declarations (to be directly called from module):
-export([ create/1, getAggregator/1, remove/0 ]).


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
%  - TraceFilename is the name of the file where traces should be written to.
%  - IsPrivate tells whether this trace aggregator will be privately held
% (hence should not be registered in naming service) or if it is a (registered) 
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
					throw( trace_aggregator_could_not_register )
			
			end,

			% Registers as well locally:
			true = register( ?trace_aggregator_name, self() ),
			?setAttribute(State,is_private,false)

	end,
	
	{ok,File} = file:open( TraceFilename, [write] ),

	EndState = ?setAttributes( PrivateState, [ {trace_filename,TraceFilename}, 
		{trace_file,File}, {trace_listeners,[]} ] ),
	io:format( "~s Trace aggregator created.~n", [ ?LogPrefix ] ),
	EndState.

	
	
% Overridden destructor.
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
	
	ok = file:close( ?getAttr(trace_file) ),

	% Then call the direct mother class counterparts: (none)
	
	io:format( "~s Trace aggregator deleted.~n", [ ?LogPrefix ] ),
	
	% Allow chaining:
	State.
	
	

	
	
% Methods section.


% Sends a full trace to this aggregator to have it stored.
% The nine fields correspond to the ones defined in our trace format.
% (const oneway)
send( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ) ->
	
	Trace = lists:flatten( io_lib:format( "~w|~s|~s|~w|~s|~s|~s|~w|~s~n", 
		[ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message  ] ) ),	
		
	io:format( ?getAttr(trace_file), "~s", [Trace] ),
	
	Listeners = ?getAttr(trace_listeners),
	case length(Listeners) of 
		
		0 ->
			ok;
			
		_Other ->	
			BinTrace = list_to_binary(Trace),
			lists:foreach( 
				fun(Listener) ->	
		
					Listener ! {addTrace,BinTrace}
				
				end,
				Listeners )
			
	end,		 
	?wooper_return_state_only( State ).


% Adds specified trace listener to this aggregator.
% (oneway)
addTraceListener(State,ListenerPid) ->
	io:format( "~s Adding trace listener ~w, and sending it previous traces.~n",
		[ ?LogPrefix, ListenerPid ] ),
	% Transfer file:
	TraceFilename = ?getAttr(trace_filename),
	Bin = file_utils:file_to_zipped_term( TraceFilename ),
	ListenerPid ! {trace_zip,Bin,TraceFilename},
	RegisterState = ?appendToAttribute(State,trace_listeners,ListenerPid),
	?wooper_return_state_only( RegisterState ).


% Removes specified trace listener from this aggregator.
% (oneway)
removeTraceListener(State,ListenerPid) ->
	io:format( "~s Removing trace listener ~w.~n", 
		[ ?LogPrefix, ListenerPid ] ),
	UnregisterState = ?deleteFromAttribute(State,trace_listeners,ListenerPid),
	?wooper_return_state_only( UnregisterState ).
	

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
% Parameter is a boolean that tells whether the aggregator should be created
% if not available (if true) or if this method should just return a failure
% notification (if false).
% Note: to avoid race conditions between concurrent calls to this static 
% method (ex: due to trace emitter instances), a simulation might start with
% a call to this method with a blocking wait until the aggregator pops up in
% registry services.
% Waits a bit before giving up: useful when client and aggregator processes are
% launched almost simultaneously.
% (static)
getAggregator(CreateIfNotAvailable) ->
	% Only dealing with registered managers (instead of using directly
	% their PID) allows to be sure only one instance (singleton) is
	% being used, to avoid the case of two managers being launched at
	% the same time (the second will then terminate immediately).
	
	% If launching multiple trace emitters in a row, first emitter may 
	% trigger the launch of trace aggregator, but second emitter might do the
	% same if the aggregator is still being initialized: 
	case basic_utils:wait_for_global_registration_of( ?trace_aggregator_name )
			of
	
		AggregatorPid when is_pid(AggregatorPid) ->
			% Already available, return its PID:
			AggregatorPid;
		
		{registration_waiting_timeout,?trace_aggregator_name} ->
		
			case CreateIfNotAvailable of 
			
				true ->
				
					% Not available, launch it synchronously (with default
					% settings):
					create(true),
					case basic_utils:wait_for_global_registration_of(
							?trace_aggregator_name ) of
	
						LaunchedAggregatorPid 
								when is_pid(LaunchedAggregatorPid) ->
							LaunchedAggregatorPid;	

						{registration_waiting_timeout,?trace_aggregator_name} ->
							error_logger:error_msg( 
								"class_TraceAggregator:getAggregator "
								"unable to launch successfully "
								"the aggregator.~n" ),
						trace_aggregator_launch_failed
						
					end;
					
				false ->			
					% Not available and not to be started:
					trace_aggregator_not_found
			
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

