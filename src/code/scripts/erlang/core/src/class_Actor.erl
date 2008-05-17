% Actor base class.
% See class_Actor_test.erl
-module(class_Actor).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
% These are class-specific data needing to be set in the constructor:
-define(wooper_construct_parameters,ActorName).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).


% Method declarations.
-define(wooper_method_export,
	simulationStarted/2,beginTick/2,timeManagerShutdown/1,actorMessage/4,
	acknowledgeMessage/2,act/1,
	getSimulationTick/1,getSimulationDate/1,getTextualTimings/1).


% Helper functions:
-export([prepare_processing_of_last_tick_messages/1,
	repost_requests/1,send_end_of_tick_notification/1,send_actor_message/3,
	manage_end_of_tick/1]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For defines:
-include("class_Actor.hrl").

% For time_manager_name:
-include("class_TimeManager.hrl").



% Implementation notes:
%
% - links/monitors communicating actors to avoid a crash of one actor results 
% in others being frozen ?


	

% Constructs a new actor.
%  ActorName is a human-readable name for that actor. It is preferably
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, ActorName ),

	% Then the class-specific actions:
	?send_trace([ TraceState, "Creating a new actor." ]),
	
	% termination_mode is either 'done' or 'terminated':
	StartingState = ?setAttributes( TraceState, [ {current_time_offset,0}, 
		{actor_message_sent,false},	{waited_acks,[]}, {pending_messages,[]},
		{termination_mode,done},
		{trace_categorization,?TraceEmitterCategorization} ] ),
		
	% Find the time manager and subscribe:
	case utils:wait_for_global_registration_of( ?time_manager_name ) of

		TimeManagerPid when is_pid(TimeManagerPid) ->
		
			?send_debug([ StartingState,"Time manager found, subscribing." ]),
				
			TimeAwareState =
				?setAttribute(StartingState,time_manager_pid,TimeManagerPid),
				
			TimeManagerPid ! { subscribe, [], self() },
			
			% Hijack the WOOPER main loop to force message selection:
			receive
			
				{wooper_result,time_subscribed} ->
					?send_debug([TimeAwareState,
						"Time subscription succeeded, actor created."]),
					TimeAwareState;
					
				{wooper_result,already_time_subscribed} ->
					?send_error([TimeAwareState,"Actor constructor failed: "
						"already subscribed to time manager, stopping actor."]),
					self() ! delete,
					TimeAwareState
					
			after 5000 ->
				?send_error([ TimeAwareState, "Actor constructor failed: "
					"time-out while waiting for time manager, "
					"stopping actor." ]),
				self() ! delete,
				TimeAwareState						
			end;
	
		Error ->
			?send_error([ StartingState, io_lib:format( 
				"Actor constructor failed: "
				"unable to find time manager: ~w, stopping actor.",	[Error]) ]),
			self() ! delete,
			StartingState	
	
	end.
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting actor." ]),
	% erlang:unlink() not used. 
	?debug([ "Actor deleted." ]),
	
	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	
	

% Methods section.


% Management section of the actor.


% Oneway sent by the TimeManager on simulation begin.
simulationStarted(State,StartingTick) ->
	?trace([ io_lib:format( "Synchronized with time manager at tick ~B.", 
		[ StartingTick ] ) ]),
	?wooper_return_state_only(?setAttribute(State,starting_time,StartingTick)).


clean() ->
	receive
	
		_ ->
			clean()
	
	after 0 ->
		ok
	
	end.
	


% Oneway sent by the TimeManager when it incremented its current tick.
beginTick(State,NewTick) ->

	% First check that it is the expected tick, and update the internal one:
	ExpectedTick = class_TraceEmitter:get_current_tick(State)+1,
	
	
	case NewTick of 
	
		ExpectedTick ->
		
			% If the termination of this actor was requested during last tick, 
			% then it must be applied now:
			case ?getAttr(termination_mode) of
			
				terminated -> 
					?trace([ io_lib:format( "Actual termination of actor "
						"occurred at tick ~B.~n", [ ExpectedTick ] ) ]),
					clean(),	
					self() ! delete,
					% Nothing more to send to the TimeManager:
					?wooper_return_state_only(State);
					
				done ->
												
					?debug([ io_lib:format( "Starting new tick at ~B.",
						[ NewTick ] ) ]),
	
					% Attribute actor_message_sent already set to false.
					
					UpdatedState = ?addToAttribute(State,current_time_offset,1),
		
					% Then reorder and repost accordingly the messages received
					% during last tick, so that they are method calls:
					RepostedState = 
						prepare_processing_of_last_tick_messages(UpdatedState),
			
					% Then, once all messages have been resent, calls the 
					% (probably overriden) 'act' method with the newer state
					% (this will be executed last):
					self() ! act,

					% The notification to the TimeManager that this tick was
					% taken into account by this actor will be the result of 
					% its receiving of last inter-message acknowledgement, if 
					% at least a message is sent, or otherwise by a direct 
					% notification to be sent by the 'act' method. 
					?wooper_return_state_only( RepostedState )
		
			end;

		UnexpectedTick ->
		
			?error([ io_lib:format( "beginTick method: received tick ~B "
				"while expecting tick ~B.", [UnexpectedTick,ExpectedTick] ) ]),
				
			% Handled by WOOPER:
			erlang:exit( {synchronization_failure,ExpectedTick,UnexpectedTick,
				?getAttr(name), self()} )
	
	end.			
				
	
	
% Reacts to a notification of time manager shutdown by deleting this actor.	
timeManagerShutdown(State) ->
	?debug([ "Received a notification of time manager shutdown, "
		"requesting deletion." ]),
	self() ! delete,
	?wooper_return_state_only(State). 

	

% The core of the actor behaviour.
% Default implementation, made to be overriden.
% (oneway)
act(State) ->
	?trace([ io_lib:format( "Acting." ) ]),
	% By default a 'done' notification will be sent:
	?wooper_return_state_only( manage_end_of_tick(State) ). 



% Oneway called by another actor to send to this actor a behaviour-specific
% message: this actor stores this message for later processing, but acknowleges
% it immediately.
% 'Message' can be either '{Method,Parameters}' or 'Method', depending on 
% whether the oneway takes parameters or not.
actorMessage(State,ActorTick,Message,ActorPid) ->

	% Only very few actor messages should arrive before the notification of 
	% tick begin while being still emitted that same tick (case of very 
	% reactive actor sending a message to an actor still waiting for the tick
	% notification). Anyway there may be as well messages scheduled for a more
	% remote future.
	CurrentTick = class_TraceEmitter:get_current_tick(State),
	case ActorTick - CurrentTick of
	
		% Usual case: sender and receiver in the same tick, thus 
		% ActorTick = CurrentTick+1:
		1 ->
			ok;
			
		% ActorTick > CurrentTick+1: receiving a message expected to be delayed:
		Value when Value > 1 ->
			?error([ io_lib:format( "Actor message received for future times "
				"(sender: ~w, message tick: ~B).",[ ActorPid, ActorTick] ) ]);
		
		% Here ActorTick-CurrentTick<1, abnormal:
		_ ->		
			?error([ io_lib:format( "Actor message received for time "
				"in the past (sender: ~w, message tick: ~B).",
				[ ActorPid, ActorTick] ) ])
		
	end,
		
	% Acknowledges to the sender:
	ActorPid ! {acknowledgeMessage,self()},
	
	% Stores this message for later processing:
	?wooper_return_state_only( ?appendToAttribute( State, pending_messages,
		{ActorTick,ActorPid,Message} ) ).

		

% Callback triggered by the reception of an acknowledgement of an actor to
% which this actor sent a message.
% (oneway)
acknowledgeMessage(State,CalledActorPid) ->

	Waited = ?getAttr(waited_acks),
	
	% Check we are waiting for this ack:
	case lists:member(CalledActorPid,Waited) of
	
		true ->
			
			% Yes, remove it from list, see if it was the last waited one:
			ShortenWaitedList = lists:delete(CalledActorPid,Waited),
			case ShortenWaitedList of
				
				[] ->
					% Last ack received, ready to declare this actor's end of
					% tick, either done or terminated:
					send_end_of_tick_notification(State);
					
				_ ->
					% There is still at least one waited ack, still waiting:
					ok
			end,	
			?wooper_return_state_only(
				?setAttribute(State,waited_acks,ShortenWaitedList));

		false ->
			exit( {unexpected_ack,CalledActorPid,Waited} )
			
	end.	
	
			
% Returns the current simulation time of this actor, in virtual gregorian
% seconds.
getSimulationTick(State) ->
	?wooper_return_state_result( State,
		class_TraceEmitter:get_current_tick(State) ).


% Returns the current simulation time of this actor, structured as follows:
% {{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond}}
getSimulationDate(State) ->
	?wooper_return_state_result( State, 
		calendar:gregorian_seconds_to_datetime(
			class_TraceEmitter:get_current_tick(State) ) ).


% Returns a textual description of the simulation and real time, for this actor.
getTextualTimings(State) ->
	?wooper_return_state_result( State, get_textual_timings(State) ).


		
	
% 'Static' methods (module functions):



% Section for helper functions (not methods).
		

	
% Returns a textual description of the real and simulated time.	
get_textual_timings(State) ->

	CurrentTick = class_TraceEmitter:get_current_tick(State),
	{{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond}} =
		calendar:gregorian_seconds_to_datetime( CurrentTick ),
	{{RealYear,RealMonth,RealDay},{RealHour,RealMinute,RealSecond}} =
		{date(),time()},
			
	io_lib:format( "actor simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[SimDay,SimMonth,SimYear,SimHour,SimMinute,SimSecond,CurrentTick,
		RealDay,RealMonth,RealYear,RealHour,RealMinute,RealSecond] ).



% Sends specified message to specified actor, records the sending to wait for 
% its acknowledgement, and returns an updated state.
%
% The sent message corresponds to a oneway, not a request, to avoid blocking
% operations.
%
% The sender PID is automatically added, thus it does not need to be specified
% explicitly in the method call.
%
% The specified tick is the one expected for the delivery, most often the next
% tick, hence the +1.
%
% The ActorMessage parameter must be understandable as a oneway call, i.e. must
% be either like 'oneway_name', '{oneway_name,SingleParameter}' or
% '{oneway_name,[Parameter1,Parameter2,..]}'.
% The associated method signature must be 'oneway_name(State,SenderPid)',
% 'oneway_name(State,SingleParameter,SenderPid)' or
% 'oneway_name(State,[Parameter1,Parameter2,..],SenderPid)'
%
% Returns an updated state, appropriate to wait for this call acknowledgement.
% (helper function)
send_actor_message(ActorPid,ActorMessage,State) ->
	ActorPid ! {actorMessage,
		[class_TraceEmitter:get_current_tick(State)+1,ActorMessage,self()]},
	SentState = ?setAttribute(State,actor_message_sent,true),
	?appendToAttribute(SentState,waited_acks,ActorPid).



% Manages for this actor the end of current tick.
%
% This includes notably ensuring that the TimeManager will be notified by this
% actor of its end of tick: if this actor sent at least one actor message, the
% last acknowledgement of of its message(s) will trigger the TimeManager 
% notification automatically, otherwise it has to be performed directly. 
%
% This function is expected to be called at the end of the act method,
% usually with '?wooper_return_state_only( 
%   class_Actor:manage_end_of_tick( AState ) )'.
% Returns an updated state.
manage_end_of_tick(State) ->
	% In all cases a notification must have been sent and actor_message_sent
	% must have been set back to false:
	case ?getAttr(actor_message_sent) of
	
		true ->
			% Prepare for next tick:
			?setAttribute(State,actor_message_sent,false);
			
		false ->	
			% actor_message_sent left to false for next tick:
			send_end_of_tick_notification(State),
			State
	
	end.
	
			

% Makes this actor search for its messages associated to its current tick,
% sort them in a particular order, and request to process them.
% May update the termination_mode attribute.
% Returns an updated state.
prepare_processing_of_last_tick_messages(State) ->
	
	% Pending messages are an unordered list of {Tick,Pid,Msg} elements. 
	% Sorts the incoming messages based first on the tick (element 1), then 
	% on the sender PID (element 2):
	% (if ever an actor had sent more than one message to this actor on the
	% same tick, orders them first by messages - element 3 - so that the
	% repeatability is ensured)
	%SortedMessages = lists:keysort( 1, lists:keysort( 2, 
	%	lists:keysort( 3, ?getAttr(pending_messages) ) ) ),

	CurrentTick = class_TraceEmitter:get_current_tick(State),
	
	% Actually messages are already checked at reception:
	{CurrentMessages,NextMessages,OutOfRangeMessages} = 
		sort_messages( ?getAttr(pending_messages), CurrentTick ),
	
	% They come from the past:
	case OutOfRangeMessages of
	
		[] ->
			ok;
		
		_ ->
			?error([ io_lib:format( 
				"There is at least one actor message in the past: "
				" ~w.",	[ OutOfRangeMessages ] ) ]),
			exit( actor_message_in_the_past )	
	
	end,
		
	% Sends the relevant requests in-order:
	repost_requests( CurrentMessages ),
	
	% Flush messages except the ones in the future:
	?setAttribute(State,pending_messages,NextMessages).	
			
		
			
% Sends in turn the specified oneways (no third parameter as requests, even 
% though the sender PID is specified, like requests) to this same process:
repost_requests([]) ->
	ok;
	
repost_requests( [ {_,SenderPid,{Method,Parameters}} | MessageTuples ] ) ->
	self() ! {Method,[Parameters,SenderPid]},
	repost_requests(MessageTuples);

repost_requests( [ {_,SenderPid,Method} | MessageTuples ] ) ->
	self() ! {Method,SenderPid},
	repost_requests(MessageTuples).

	
	
% Notifies the time manager that the current tick is over for this actor, and
% that this actor is terminating or not, depending on its termination
% attribute.
send_end_of_tick_notification(State) ->
	?getAttr(time_manager_pid) !
		{ ?getAttr(termination_mode),
			{class_TraceEmitter:get_current_tick(State),self()} }.



% Sorts the specified list of messages into three lists (returned as a tuple):
% the messages corresponding to the specified tick, the ones in the future of
% that tick, and the ones in its past.
sort_messages(Messages,Tick) ->
	sort_messages(Messages,Tick,[],[],[]).
	
	
sort_messages([],_,Current,Future,Past) ->
	{Current,Future,Past};

sort_messages([{MessageTick,Pid,Message}|T],Tick,Current,Future,Past) 
		when MessageTick < Tick ->
	sort_messages(T,Tick,Current,Future,[{MessageTick,Pid,Message}|Past]);
	
sort_messages([{MessageTick,Pid,Message}|T],Tick,Current,Future,Past) 
		when MessageTick > Tick ->
	sort_messages(T,Tick,Current,[{MessageTick,Pid,Message}|Future],Past);

% Here 'when MessageTick == Tick' is implied: 
sort_messages([{MessageTick,Pid,Message}|T],Tick,Current,Future,Past) ->
	sort_messages(T,Tick,[{MessageTick,Pid,Message}|Current],Future,Past).
	 
