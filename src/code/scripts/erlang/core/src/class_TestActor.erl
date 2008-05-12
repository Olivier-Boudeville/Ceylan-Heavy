% Test of Actor class, regarding time management.
-module(class_TestActor).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,ActorName,TerminationTickOffset).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).

% Method declarations.
-define(wooper_method_export,act/1,addPeer/2,removePeer/2,hello/3).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Test").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


	
% Constructs a new test actor.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorName ),

	?send_info([ ActorState, "Creating a new test actor." ]),
	
	?setAttributes( ActorState, [ {peers,[]}, 
		{termination_tick_offset,TerminationTickOffset},
		{trace_categorization,?TraceEmitterCategorization} ] ).
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting test actor." ]),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Actor deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).
	
	
	

% Methods section.


% Management section of the actor.
	
				

% The core of the test actor behaviour.
% (oneway)
act(State) ->

	?info([ "Test Actor acting" ]),
	
	TerminationOffset = ?getAttr(termination_tick_offset),
	
	% Terminates if the termination offset is reached:		
	UpdatedState = case ?getAttr(current_time_offset) of 
	
		TerminationOffset ->
		
			?info([ "Test Actor preparing termination." ]),
					
			TerminatedState = ?setAttribute(State,termination_mode,terminated),
				
			% Peers must be notified here, otherwise, next time they will send
			% to this actor a message, they will hang forever.
			% This returns a new state:
			notify_termination(TerminatedState);	
			
		_ ->
			% This returns a new state:
			say_hello(State)
			
	end,
	
	% If no message has been sent (i.e. no peer is known), the end of
	% tick notification must be sent explicitly:
	% (both previous cases send message iff there is at least a peer)
	case ?getAttribute(UpdatedState,peers) of
			
		Peers when length(Peers) > 0 ->
			ok;
				
		_ ->	
			% No actor message sent here, thus having to call:
			class_Actor:send_end_of_tick_notification(UpdatedState)
					
	end,	
	?wooper_return_state_only(UpdatedState).
			



% Adds specified peer to known peers.
% (oneway helper, note that there is not repeatable mechanism underneath)
addPeer(State,PeerPid) ->
	?wooper_return_state_only( ?appendToAttribute(State,peers,PeerPid) ). 


% Oneway called by a peer requesting this actor not to send it anymore messages,
% for example because this actor is terminating.
removePeer(State,PeerPid) ->
	?wooper_return_state_only( ?deleteFromAttribute(State,peers,PeerPid) ). 


% Receive a hello message.	
hello(State,SenderName,SenderPid) ->
	?info([ io_lib:format( "Received an hello message from ~s (~w).",
		[ SenderName, SenderPid ] ) ]),
	?wooper_return_state_only(State). 
		
	
	
% Section for helper functions (not methods).

	
	
% Says hello to all peers.
say_hello(State) ->
	F = fun( Fun, PeerList, FunState ) ->
	
		case PeerList of
			
			[] ->
				FunState;
			
			[H|T] ->
				?info([ io_lib:format( "Sending hello to ~w.", [ H ] ) ]),
				Fun(Fun,T, class_Actor:send_actor_message(
					H,{hello,?getAttr(name)},FunState) )
					
		end		
	
	end,
	% Returns an updated state:
	F(F,?getAttr(peers),State).



% Notify all peers that this actor is terminating, thus they must not send
% anymore messages to it.	
notify_termination(State) ->
	F = fun( Fun, PeerList, FunState ) ->
	
		case PeerList of
			
			[] ->
				FunState;
			
			[H|T] ->
				?info([ io_lib:format( 
					"Sending termination notification to ~w.", [ H ] ) ]),
				Fun(Fun,T, class_Actor:send_actor_message(
					H,removePeer,FunState) )
					
		end		
	
	end,
	% Returns an updated state:
	F(F,?getAttr(peers),State).

