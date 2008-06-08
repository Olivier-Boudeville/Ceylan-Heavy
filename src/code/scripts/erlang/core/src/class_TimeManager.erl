% Management of simulation time.
% The time manager is expected to be a singleton.
% Its process is registered under the name ?time_manager_name.
% This is a time-driven simulation, actors are expected to be synchronous.
% See class_TimeManager_batch_test.erl and 
% class_TimeManager_interactive_test.erl.
-module(class_TimeManager).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
% Interactive is true or false, respectively for interactive mode and batch one.
-define(wooper_construct_parameters,SimulationFrequency,Interactive).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).


% Method declarations.
-define(wooper_method_export,
	start/1,start/2,start/3,stop/1,timer_top/1,done/2,terminated/2,
	getSimulationTick/1,getSimulationDate/1,getTextualTimings/1,
	convertTicksToSeconds/2,convertSecondsToTicks/2,
	subscribe/1,unsubscribe/1,
	onWooperExitReceived/3,
	display/1,toString/1).


% Static method declarations (to be directly called from module):
-export([ create/0,create/1,create/2,remove/0,
	start/0,startUntil/1,startUntil/2,stop/0,
	getManager/0,getSimulationTick/0,
	convertTicksToSecondsWith/2,convertSecondsToTicksWith/2,
	subscribe/0,unsubscribe/0]).


% Helper functions:
-export([get_current_tick/1,begin_new_tick/1,handle_end_of_tick_for/4]).


% For the timing internal process:
-export([count_time/2]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"TimeManagement").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For talkative settings:
-include("simulation_settings.hrl").

% For time_manager_name:
-include("class_TimeManager.hrl").


% Use global:registered_names() to check manager presence.


% Implementation notes:
%
%  - timer_pid is set iff in interactive mode and running (not stopped)


% Constructs a new time manager.
%  - SimulationFrequency designates the number of time slices in each 
% virtual second (not supported yet)
%  - Interactive can be true (then the simulation will try to run on par with 
% the user time), or false (then the simulation will run as fast as possible,
% regardless of user time)
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, "TimeManager" ),
		
	CategorizedState = ?setAttribute( TraceState, trace_categorization,
		?TraceEmitterCategorization ),
		
	?send_trace([ CategorizedState, io_lib:format( 
		"Creating time manager with simulation frequency ~w Hz.", 
		[ SimulationFrequency ] ) ]),
		
	case global:register_name( ?time_manager_name, self() ) of 
	
		yes ->
			?send_debug([ CategorizedState, 
				"Time manager registered globally" ]);
		
		no ->
			?send_error([ CategorizedState, 
				"Time manager could not be registered." ]),	
			exit( time_manager_could_not_register )
			
	end,
	
	% Registers as well locally:
	true = register( ?time_manager_name, self() ),
	
	% Simulation period, in millisecond.
	% It is the number of virtual milliseconds between two simulation ticks.
	% (20 ms corresponds to 50 Hz)
	TickDuration = round( 1000 / SimulationFrequency ),
	
	% The real (i.e. wall-clock, user) duration, in milliseconds, expected
	% between two simulation ticks, when in interactive mode, is defined in
	% the simulation_tick_waiting attribute:

	StartState = ?setAttributes( CategorizedState, [ {started,false}, 
		{stop_tick,undefined}, {stop_listener,undefined},
		{requested_simulation_frequency,SimulationFrequency},
		{simulation_tick_duration,TickDuration},
		{simulation_tick_waiting,
			?scale_factor_for_interactive_time*TickDuration},
		{interactive,Interactive}, {terminated_actors,[]}, {time_listeners,[]} 
	] ),

	?send_debug([ StartState, "Time manager created." ]),
	StartState.

	
% Overriden destructor.
% All still-subscribed listeners will be warned of the manager deletion by
% a timeManagerShutdown message.
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting time manager." ]),
	
	case ?getAttr(started) of

		true ->	
			stop(State);
		
		false ->
			ok	
	
	end,
	notify_all_listeners( State, timeManagerShutdown ),
	unregister( ?time_manager_name ),
	global:unregister_name( ?time_manager_name ),
	?trace([ "Time manager deleted." ]),
	
	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	
	
	

% Methods section.


% Management section of the time manager.


% Starts the time manager with no specific termination tick defined.
% (oneway)	
start(State) ->
	% Here no termination tick is set:
	?wooper_return_state_only( init(State) ).					


% Starts the time manager with a specific termination tick defined.	
% (oneway)	
start(State,TerminationOffset) ->
	InitState = init( State ),
	StopTick = ?getAttribute(InitState,starting_time) + TerminationOffset,
	?info([ io_lib:format("Will stop at tick ~B.",[StopTick]) ]),
	?wooper_return_state_only( 
		?setAttribute( InitState, stop_tick, StopTick ) ).


% Starts the time manager with a specific termination tick defined and a
% registered stop listener.
% (oneway)	
start(State,TerminationOffset,StopListenerPID) ->
	start( ?setAttribute( State, stop_listener, StopListenerPID ),
		TerminationOffset ).
		
			 
% Stops the time manager.	
% (oneway)
stop(State) ->
	?info([ io_lib:format( "Stopping simulation clock at ~s.", 
		[get_textual_timings(State)] ) ]),

	case ?getAttr(started) of
	
		false ->
			?warning([ "Stop request ignored: simulation clock not running." ]),
			?wooper_return_state_only(State);

		true ->
		
			StoppedState = ?setAttribute(State, started, false),	

			case ?getAttribute(StoppedState,stop_listener) of

				undefined ->
					ok;
			
				ListenerPID ->
					ListenerPID ! simulation_stopped
	
			end,
	
			case ?getAttribute(StoppedState,interactive) of 

				true ->

					% If interactive, the timer has to be stopped:
					case ?hasAttribute(StoppedState,timer_pid) of 
		
						true ->
					
							?debug([ "Waiting for timer to stop." ]),
							TimerPid = ?getAttribute(StoppedState,timer_pid),
							TimerPid ! stop_timer,
					
							receive
			
								timer_stopped ->
									?debug([ "Timer stopped." ]),
									?wooper_return_state_only( 
										?removeAttribute( StoppedState,
											timer_pid ) )
				
							end;
				
						false ->
							% Nothing to stop:
							?wooper_return_state_only(StoppedState)
					
					end;


				false ->
					% In batch mode, no timer to stop:
					?wooper_return_state_only(StoppedState)


			end
			
	end.



% Section for time synchronization of operations.	


% Increases the tick of the simulation clock and notify all listeners.
% (to be called by the internal timer if interactive, otherwise directly).
% (oneway)	
timer_top(State) ->
	io:format( "YYY1"),
	%?trace([ "Waited listeners are: ~w", [?getAttr(waited_listeners)]),
	
	% Check whether the current tick can really be declared over:
	case ?getAttr(waited_listeners) of 
	
		[] ->
		
			% Yes, all actors performed their actions.
			
			% If interactive, check we are on time:
			case ?getAttr(interactive) of 
			
				true ->
					% We suppose here that this manager is not overloaded
					% enough to process this timer_top request with significant
					% delay. 
					% We just suppose this call corresponds to the
					% real time and just check that actors are on time.
					% A more robust approach would be to base at least the 
					% timer on the real time, not on receive time-outs. 
					?debug([ io_lib:format( "timer_top on time for tick ~B.", 
						[ get_current_tick(State) ] ) ]);
					
				false -> 
					% In batch mode, best-effort is always guaranteed.
					ok
					
			end,
			% Both cases ready for next tick: 
			?wooper_return_state_only( begin_new_tick(State) );
		
		NonEmptyList ->
			case ?getAttr(interactive) of 
			
				true ->
					?warning([ io_lib:format( "timer_top method: "
						"cannot keep up with interactive pace, "
						"there are still waiting listeners: ~w, "
						"loosing sync with hard real time.",[NonEmptyList])	]),
					% Tick not changed, still waiting:
					?wooper_return_state_only(State);
												
				false ->
					?error([ io_lib:format( "timer_top method: "
						"there are still waited listeners: ~w (abnormal), "
						"still waiting.", [NonEmptyList] ) ]),
					exit( still_pending_actors )	
						
			end	
					
	end.
	
	
		
% Returns the current simulation time, in virtual gregorian seconds.
% (request)
getSimulationTick(State) ->
	?wooper_return_state_result( State, get_current_tick(State) ).


% Returns the current simulation time, structured as follows:
% {{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond}}
% (request)
getSimulationDate(State) ->
	?wooper_return_state_result( State, 
		calendar:gregorian_seconds_to_datetime( 
			convert_ticks_to_seconds( State, get_current_tick(State) ) ) ).


% Returns a textual description of the simulation and real time. 
% (request)
getTextualTimings(State) ->
	?wooper_return_state_result( State, get_textual_timings(State) ).



% Converts the specified number of seconds into an integer (rounded) of ticks,
% using the specified time manager.
% Returns the appropriate number of ticks.
% (request)
convertSecondsToTicks(State,Seconds) ->
	?wooper_return_state_result( State, 
		convert_seconds_to_ticks( State, Seconds ) ).

		
% Converts the specified tick count into an integer (rounded) number of seconds,
% using the specified time manager.
% Returns the appropriate number of seconds.
% (request)
convertTicksToSeconds(State,Ticks) ->
	?wooper_return_state_result( State, 
		convert_ticks_to_seconds( State, Ticks ) ).




% Section dedicated to the subscribing/unsubscribing of time listeners, 
% generally the simulated actors.


% Requests the manager to subscribe the caller as a time listener.
% Returns either:
%   - time_subscribed if the operation succeeded (subscribed and
% was not subscribed yet). The listener will receive the begin notification for
% next tick. 
%   - already_time_subscribed if the caller was already subscribed (it is still
% subscribed only once)
% Note: could not be named 'register', as it is a reserved word.
% ex: MyTimeManager ! { subscribe, [], self() }
% Current tick of manager cannot be returned, as it may not be started yet,
% in which case there is no current tick.
%
% (request).
%
subscribe(State) ->

	io:format( "YYY3"),
	
	% PID retrieved from request:
	Caller = ?getSender(),
	
	case lists:member(Caller, ?getAttr(time_listeners) ) of 
			
		true ->
			?warning([ io_lib:format( "Subscribing requested, whereas "
				"actor ~w was already time subscribed.", [Caller] ) ]),
			?wooper_return_state_result(State,already_time_subscribed) ;
				
		false ->

			?debug([ io_lib:format( "Subscribing actor ~w.", [Caller] ) ]),
		
			% Links together this manager and the calling listener, so that the
			% termination of one will result in the other receiving an exit
			% signal:
			erlang:link(Caller), 
			
			% If manager is already started, notifies directly the actor,
			% otherwise do nothing, as it will be done when starting:
			case ?getAttr(started) of 
				
				true ->
					Caller ! { simulationStarted, get_current_tick(State) }; 
				
				false ->
					ok
			
			end,
			
			?wooper_return_state_result(
				?appendToAttribute(State,time_listeners,Caller),
				time_subscribed )
					
	end.

	

% Requests the manager to unsubscribe the caller from the list of time
% listeners.
% Returns either:
%   - time_unsubscribed if the operation succeeded (subscribed and was not 
% subscribed yet)
%   - not_already_time_subscribed if the caller was not already subscribed
% (hence nothing is to be done)
% ex: MyTimeManager ! { unsubscribe, [], self() }
unsubscribe(State) ->
	Caller = ?getSender(),
	Listeners = ?getAttr(time_listeners),
	case lists:member(Caller,Listeners) of 
			
		true ->
			?debug([ io_lib:format( "Unsubscribing actor ~w.", [Caller] ) ]),
			?wooper_return_state_result(
				?setAttribute( State, time_listeners,
					lists:delete(Caller,Listeners) ), time_unsubscribed ) ;
				
		false ->
			?warning([ io_lib:format(
				"Actor ~w was not already time subscribed, "
				"whereas unsubscribing requested.",	[Caller] ) ]),
			?wooper_return_state_result( State,	not_already_time_subscribed )
					
	end.



% Oneway method to be called by each listener having completed its tick and
% not terminating. 
done(State, {ActorTick,ActorPid} ) ->
	?wooper_return_state_only( 
		handle_end_of_tick_for(State,ActorTick,ActorPid,done) ).


% Oneway method to be called by each listener having completed its tick and
% being terminating. 
% It will be notified on next tick (so that it knows when it is not needed
% anymore), and then never again.
% (oneway).
terminated(State, {ActorTick,ActorPid} ) ->
	UpdatedState = handle_end_of_tick_for(State,ActorTick,ActorPid,terminated),
	?wooper_return_state_only( ?appendToAttribute( UpdatedState,
		terminated_actors, ActorPid ) ).	
	

% Overriding the WOOPER default EXIT handler, not interested in 'normal' EXIT:
onWooperExitReceived(State,_,normal) ->
	?wooper_return_state_only(State);
	
onWooperExitReceived(State,Pid,ExitType) ->
	?warning([ io_lib:format( 
		"TimeManager EXIT handler ignored signal '~w' from ~w.",
		[ExitType,Pid] ) ]),
	?wooper_return_state_only(State).	
	

% Generic interface.	
	
% Displays the state in the console.
display(State) ->
	wooper_display_instance(State),
	?wooper_return_state_only( State ).


% Returns a textual description of this manager.
toString(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).
	
	
	
	
	
% 'Static' methods (module functions):


% Creates the time manager asynchronously by default in interactive mode,
% using default simulation frequency.
% (static)
create() ->
	% create( Interactive )
	create( true ).
		
		
% Creates the time manager asynchronously in interactive mode (if Interactive
% is true) or in best effort mode (static), using default simulation frequency.
create(Interactive) ->
	% create( Interactive, Verbose )
	new_link( ?default_simulation_frequency, Interactive ).
		
		
% Creates the time manager asynchronously in interactive mode (if Interactive
% is true) or in best effort mode (static), with specified simulation frequency.
create(SimulationFrequency,Interactive) ->
	% create( Interactive, Verbose )
	new_link( SimulationFrequency, Interactive ).
		

	
% Starts the already-created time manager (static):
start() ->

	% Allows to create and start directly afterwards:
	case utils:wait_for_global_registration_of( ?time_manager_name ) of
	
		{registration_waiting_timeout,?time_manager_name} ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! start
			
	end.

	
	
% Starts the already-created time manager, with a termination tick specified.
% TerminationOffset is the number of ticks that should be waited until the 
% time manager stops (duration, not absolute time).
% (static)
startUntil(TerminationOffset) ->

	% Allows to create and start directly afterwards:
	case utils:wait_for_global_registration_of( ?time_manager_name ) of
	
		{registration_waiting_timeout,?time_manager_name} ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! {start,TerminationOffset}
			
	end.
	
	
% Starts the already-created time manager, with a termination tick specified,
% and specified stop listener.
% (static)
startUntil(TerminationOffset,StopListenerPID) ->

	% Allows to create and start directly afterwards:
	case utils:wait_for_global_registration_of( ?time_manager_name ) of
	
		{registration_waiting_timeout,?time_manager_name} ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! {start,[TerminationOffset,StopListenerPID]}
			
	end.
	
	

% Stops the time manager.
% (static)	
stop() ->

	case global:whereis_name( ?time_manager_name ) of
	
		undefined ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! stop
			
	end.




% Converts the specified number of seconds into an integer (rounded) of ticks,
% using the specified time manager.
% Returns the appropriate number of ticks.
% (static)
convertSecondsToTicksWith(TimeManagerPID,Seconds) ->
	TimeManagerPID ! { convertSecondsToTicks, Seconds, self() },
	% Will hang forever in case of a problem:
	receive

		% (beware, static method, thus reading the caller mailbox)

		{wooper_result,Ticks} when is_integer(Ticks) ->
			Ticks
			
	end.
	
	
% Converts the specified tick count into an integer (rounded) number of seconds,
% using the specified time manager.
% Returns the appropriate number of seconds.
% (static)
convertTicksToSecondsWith(TimeManagerPID,Ticks) ->
	TimeManagerPID ! { convertTicksToSeconds, Ticks, self() },
	% Will hang forever in case of a problem:
	receive
	
		% (beware, static method, thus reading the caller mailbox)

		{wooper_result,Seconds} when is_integer(Seconds) ->
			Seconds
			
	end.
	

% Deletes the time manager.
% (static)	
remove() ->

	case global:whereis_name( ?time_manager_name ) of
	
		undefined ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! delete
			
	end.


% Returns the Pid of the current time manager if it exists, otherwise 
% time_manager_not_found. 
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
% (static)
getManager() ->

	% Waits gracefully for the time manager to exist:
	case utils:wait_for_global_registration_of( ?time_manager_name ) of
	
		{registration_waiting_timeout,?time_manager_name} ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid
			
	end.
	


% Allows a client of the time manager to subscribe safely.
% Returns either time_manager_not_found, time_manager_no_answer,
% time_subscribed or time_manager_no_answer.
% Unsubscription can be done either by calling unsubscribe or by sending a
% 'terminated' message.
% (static)
subscribe() ->

	% (beware, static method, thus reading the caller mailbox)

	case getManager() of
	
		time_manager_not_found ->
			time_manager_not_found ;
		
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! { subscribe, [], self() },
			receive
			
				% Only two special messages searched:

				{wooper_result,already_time_subscribed} ->
					already_time_subscribed;
					
				{wooper_result,time_subscribed} ->
					time_subscribed
					
					
			after 1000 ->
				time_manager_no_answer
					
			end
	
	end.		
		
		
		
% Allows a client of the time manager to unsubscribe safely.
% Returns either time_manager_not_found, time_manager_no_answer, 
% not_already_time_subscribed, or time_unsubscribed.
% (static)
unsubscribe() ->

	% (beware, static method, thus reading the caller mailbox)

	case getManager() of
	
		time_manager_not_found ->
			time_manager_not_found ;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! { unsubscribe, [], self() },
			receive
			
				% Only two special messages searched:
				
				{wooper_result,time_unsubscribed} ->
					time_unsubscribed;
					
				{wooper_result,not_already_time_subscribed} ->
					not_already_time_subscribed
															
			after 1000 ->
				time_manager_no_answer
					
			end
	
	end.		



% Returns the current simulation tick.
% (static)
getSimulationTick() ->

	case global:whereis_name( ?time_manager_name ) of
	
		undefined ->
			time_manager_not_found;
			
		TimeManagerPid when is_pid(TimeManagerPid) ->
			TimeManagerPid ! { getSimulationTick, [], self() },
			receive	

				% (beware, static method, thus reading the caller mailbox)
	
				{wooper_result,Tick} when is_integer(Tick) ->
					Tick
						
			after 1000 ->
				time_manager_no_answer
						
			end
	end.




% Section for helper functions (not methods).
		

% Internal timer functions, for interactive mode.


% Ticks at a regular pace:
count_time(ManagerPid,TimeOut) ->
	receive 
	
		stop_timer ->
			%io:format( "Timer: count_time requested to stop.~n" ),
			ManagerPid ! timer_stopped
	
	% After following real milliseconds:		
	after TimeOut ->
		ManagerPid ! timer_top,
		count_time(ManagerPid,TimeOut)
	end.


% Returns the current (numerical) simulation tick, in virtual gregorian seconds.
% Note: the time manager must be started.
get_current_tick(State) ->
	%io:format( "get_current_tick called.~n" ),
	?getAttr(starting_time)	+ ?getAttr(current_time_offset).


% Takes care of the beginning of a new simulation tick.
% Returns an updated state.
begin_new_tick(State) ->

	% Here we go for next tick:					
	NewState = ?addToAttribute( State, current_time_offset, 1 ),
	
	TimeListeners = ?getAttr(time_listeners),
	
	?info([ io_lib:format( "Top: ~s, sent to ~B actor(s).", 
		[ get_textual_timings(NewState), length( TimeListeners ) ] )
	]),
	
	CurrentTick = get_current_tick(NewState),
	
	case ?getAttr(stop_tick) of
	
		CurrentTick ->
			?info([ io_lib:format( "Reached termination tick ~B, stopping.",
				[ CurrentTick ] ) ]),
			self() ! stop,
			NewState;
		
		_NonTerminalTick ->
		
			case TimeListeners of 
			
				[] ->
				
					?debug([ "No actor registered currently, nothing to do." ]),
					% Light sleeping added here, to avoid to eat ticks at 
					% light speed:
					timer:sleep(10),
					self() ! timer_top,
					NewState;
				
				_ExistingListeners ->	
				
					% Other tick or undefined:
					% Can be interpreted as a listener one-way call:	
					% (sends the beginTick tuple to all of them)
					notify_all_listeners( NewState, {beginTick,CurrentTick} ),
		
					Terminators = ?getAttribute(NewState, terminated_actors),
	
					% Unlinks actors on termination:
					% Yes, the Fun parameter is needed for recursion with 
					% anonymous functions:  
					F = fun( Fun, ListOfTerminators ) ->

						case ListOfTerminators of
		
							[] ->
								ok;
				
							[H|T] ->
								erlang:unlink( H ),
								Fun(Fun,T)	
				
						end		
		
					end,	
					F(F,Terminators),
		
					% Removes all actors having sent a terminated message during
					% last tick:
					FilteredListeners = lists:subtract( TimeListeners,
						Terminators ),
	 
					% Prepares to wait for all remaining listeners and clears
					% the terminated list, now that used:
					?setAttributes( NewState, 
					[   {time_listeners,FilteredListeners},
		  				{waited_listeners,FilteredListeners}, 
		  				{terminated_actors,[]} ] )
			end			
					
	end.
		

	


% Performs operations common to all the types of end of tick: 'done' or
% 'terminated'.
handle_end_of_tick_for( State, ActorTick, ActorPid, NotificationType ) ->
		
	% Checks that actor and manager ticks match:	
	case get_current_tick(State) of
	
		% Checks we were waiting for that actor:
		ActorTick -> 
		
			?debug([ io_lib:format( 
				"Received a notification of end of tick ('~w') "
				"for PID ~w at right tick ~B.",
				[NotificationType,ActorPid,ActorTick] ) ]),
			
			WaitedList = ?getAttr(waited_listeners),
				
			case lists:member(ActorPid,WaitedList) of
			
				true ->
					
					% Not waited anymore:
					UpdatedList = lists:delete(ActorPid,WaitedList),
					UpdatedState = ?setAttribute(State, waited_listeners,
						UpdatedList),
					
					% Still waiting for others ?	 
					case UpdatedList of
					
						[] ->
							?debug([ io_lib:format( 
								"With '~w' notification received from ~w, "
								"all 'end of tick' notifications received, "
								"tick ~B ended.", 
								[ NotificationType, ActorPid, ActorTick ] ) ]),
							
							case ?getAttr(started) of 
							
								false ->
									% Here the TimeManager was stopped during 
									% this tick, do not trigger next tick:
									?debug([ "Not triggering a new top." ]),
									ok;
									
								true ->
									% TimeManager still running.
									% Go to next tick if not interactive.
									% If interactive, will wait until the timer
									% sends the top itself: 
									case ?getAttribute( UpdatedState,
											interactive ) of 
							
										true ->
											ok;
								
										false ->	
											self() ! timer_top
									
									end		
							
							end;
							
							
						NonEmptyList ->	
							?debug([ io_lib:format( 
								"'~w' notification received from ~w, "
								"still waiting for ~w.", 
								[NotificationType,ActorPid,NonEmptyList ] )	])
					
					end,
					UpdatedState;
							
				false ->
					?error([ io_lib:format( 
						"'~w' notification received from actor ~w, "
						"whereas not waited for. Notification ignored.",
						[NotificationType,ActorPid] ) ]),
					State
					
			end;
			
		CurrentTick when CurrentTick > ActorTick ->
			?error([ io_lib:format( 
				"'~w' notification whose tick is in the past received "
				"but ignored: actor is ~w, time manager is at tick ~B, "
				"whereas actor tick is ~B.",
				[NotificationType,ActorPid,CurrentTick,ActorTick] ) ]),
			State;
			
		CurrentTick when CurrentTick < ActorTick ->
			?error([ io_lib:format( 
				"'~w' notification whose tick is in the future received "
				"but ignored: actor is ~w, time manager is at tick ~B, "
				"whereas actor tick is ~B.",
				[NotificationType,ActorPid,CurrentTick,ActorTick] ) ]),
			State;
				
		_ ->
			?error([ "handle_end_of_tick_for: "
				"unexpected case occurred, ignored." ]),
			State
			
	end.
			
	
% Returns a textual description of the real and simulated time.	
get_textual_timings(State) ->
	CurrentTick = get_current_tick(State),
	{{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond}} =
		calendar:gregorian_seconds_to_datetime( 
			convert_ticks_to_seconds( State, CurrentTick ) ),
	{{RealYear,RealMonth,RealDay},{RealHour,RealMinute,RealSecond}} =
		{date(),time()},
			
	io_lib:format( "simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[SimDay,SimMonth,SimYear,SimHour,SimMinute,SimSecond,CurrentTick,
		RealDay,RealMonth,RealYear,RealHour,RealMinute,RealSecond] ).



% Sends a message to all time listeners.
notify_all_listeners(State,Message) ->
	%?debug([ io_lib:format( "All ~B listener(s) will be notified of ~w.",
	%	[length(?getAttr(time_listeners)),Message] ) ]),
	warn_listeners( ?getAttr(time_listeners), Message ).
	

warn_listeners( [Listener|Others], Message ) ->
	Listener ! Message,
	warn_listeners(Others,Message);
	
warn_listeners([],_) ->
	ok.


% Converts the specified number of seconds into an integer (rounded) of ticks.
convert_seconds_to_ticks(State,Seconds) ->
	round( Seconds * 1000 / ?getAttr(simulation_tick_duration) ).


% Converts the specified tick count into an integer (rounded) number of seconds.
convert_ticks_to_seconds(State,Ticks) ->
	round( Ticks * ?getAttr(simulation_tick_duration) / 1000 ).



% Init helper function used by all start methods.
% Returns an updated state.
init(State) ->

	% This manager will not terminate when an exit signal is received, instead
	% the signal will be transformed into an 'EXIT' message: 
	% (a monitor could be used instead)
	erlang:process_flag( trap_exit, true ),
	
	% Not starting at user time anymore, as it may break reproducibility:
	%{Date,Time} = {date(), time()},
	
	% Starting at a base (common) date instead, 1/1/2008 at midnight:
	{Date,Time} = {{2008,1,1},{0,0,0}},
	
	StartedState = ?setAttributes( State, [
		{stop_tick,undefined}, {started,true},
		{starting_time, convert_seconds_to_ticks( State,
			calendar:datetime_to_gregorian_seconds( {Date,Time} ) ) } ,
		{waited_listeners,[]}, {current_time_offset,0} ]),
	
	% Notifying all actors already registered that the simulation starts:
	notify_all_listeners( StartedState,
		{ simulationStarted, get_current_tick(StartedState) } ),
	
	case ?getAttribute(StartedState,interactive) of 
	
		true ->
			
			case ?hasAttribute(StartedState,timer_pid) of 
	
				true ->
					?warning([ "Start request ignored: "
						"simulation clock already running." ]),
					% State, not StartedState:	
					State;

				false ->
					?notify_by_speak( 
						"Starting simulation clock in interactive mode." ),
					
					?notify_mute( io_lib:format( 
						"Starting simulation clock in interactive mode "
						"at ~s with a requested simulation frequency "
						"of ~B Hz (period of ~B ms).", 
						[ get_textual_timings(StartedState),
						 ?getAttr(requested_simulation_frequency),
						 ?getAttr(simulation_tick_duration) ] ) ),

					% Starts and sets timer_pid:
					?setAttribute(StartedState, timer_pid, 
						spawn_link( ?MODULE, count_time,
							[self(),?getAttr(simulation_tick_waiting)] ) )
		
			end;
			
		false ->
	
			?notify_by_speak( "Starting simulation clock in batch mode." ),
			
			?notify_mute( io_lib:format( "Starting simulation clock in batch "
				"(non-interactive mode) at ~s with a simulation frequency "
				"of ~B Hz (period of ~B ms).", 
				[ get_textual_timings(StartedState),
					?getAttr(requested_simulation_frequency), 
					?getAttr(simulation_tick_duration) ] ) ),
				
			% Sends the first top:	
			self() ! timer_top,
			io:format( "YYY0"),
			StartedState	
			
	end.
	
