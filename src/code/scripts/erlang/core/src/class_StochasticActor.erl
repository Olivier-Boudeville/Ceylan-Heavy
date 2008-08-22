% Stochastic Actor class, i.e. actor whose behaviour is at least partly ruled
% by random values .
% See class_StochasticActor_test.erl
-module(class_StochasticActor).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,StochasticActorName,ListOfRandomLists).

% Life-cycle related exported operators:
-define( wooper_construct_export, new/2, new_link/2,
	synchronous_new/2, synchronous_new_link/2, construct/3, delete/1 ).

% Method declarations.
-define( wooper_method_export, setReadyListener/2, setUniformValues/3,
	setExponentialValues/3, setPositiveIntegerExponentialValues/3,
	setGaussianValues/3, setPositiveIntegerGaussianValues/3 ).


% Helper functions:
-export([ initialize_random_buffers/1, get_random_value_from/2 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.StochasticActor").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").

	
% Implementation notes:
%
% Each stochastic actor uses the common random manager to refill its cached
% random lists.


% Constructs a new stochastic actor.
%   - StochasticActorName the name of this stochastic actor
%   - ListOfRandomLists is a list of triplets, each triplet being in the 
%form of: { attribute_name_of_list, RandomType, MaxDrawnPerTick }
%     * attribute_name_of_list is a list identifier, it can be chosen freely
% (avoid collision though !). Ex: my_first_uniform_list
%     * RandomType designates the random law (ex: uniform, exponential,
% gaussian, etc.)
%     * MaxDrawnPerTick corresponds to an upper bound to the number of
% values that may be drawn during one tick for this actor and this distribution.
% For example: { { test_first_uniform, {uniform, 5}, 10} }
% See also: get_random_value_from/2 for more details
construct(State,?wooper_construct_parameters) ->
	
	% First the direct mother classes:
	ActorState = class_Actor:construct(State,StochasticActorName),

	% Then the class-specific actions:
	StartingState = ?setAttributes( ActorState, [ {ready_listener,none},
		{trace_categorization,?TraceEmitterCategorization} ] ),

	?send_trace([ StartingState, "Creating a new stochastic actor." ]),
	
	% initialization_status starts with 'blank', first action of the 'act'
	% method should be to request list refill.
	% See class_TestStochasticActor.erl
	PreparedState = prepare_random_lists( ListOfRandomLists, StartingState ),
		 	
	% Needing the random manager:
	case basic_utils:wait_for_global_registration_of( ?random_manager_name ) of

		RandomManagerPid when is_pid(RandomManagerPid) ->
			?send_trace([ PreparedState, "RandomManager found." ]),
			?setAttribute( PreparedState,random_manager_pid,RandomManagerPid );
						
		Error ->
			?send_error([ PreparedState, io_lib:format( 
				"Stochastic actor constructor failed: "
				"unable to find random manager: ~w, stopping actor.", 
				[Error]) ]),

			self() ! delete,
			PreparedState
	
	end.
	
	
	
% Overriden destructor.
% Unsubscribing from TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting stochastic actor." ]),
	% erlang:unlink() not used. 
	?debug([ "Stochastic actor deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).
	
	
	

% Methods section.


% Management section of the actor.


% Sets the ready listener for this stochastic actor. It will be told as soon
% as this actor is ready.
% (actor oneway)
setReadyListener(State,ListenerPID) ->
	none = ?getAttr(ready_listener),
	NewState = case ?getAttr(initialization_status) of
	
		completed ->
			class_Actor:send_actor_message( ListenerPID, notifyReady, State );
 
 		_ ->
			% Not yet completed, will notify the listener on completion:
			?setAttribute(State,ready_listener,ListenerPID)
	
	end,
	?wooper_return_state_only(NewState).
	


% Called by the random manager, in answer to a getUniformValues call.
% (actor oneway)
setUniformValues(State,[RequestIdentifier,Values],_SenderPid) ->

	?debug([ io_lib:format( 
		"setUniformValues called for list ~w with values ~w.", 
		[ RequestIdentifier, Values ] ) ]),

	% Ex: {[],{uniform,100},4} ({uniform,N} could replace RandomType)
	{CurrentRandomList,RandomType,MinSize} = ?getAttr(RequestIdentifier),
	
	% Now that random lists are being filled, notify any ready listener:
	ReadyState = case ?getAttr(ready_listener) of
	
		none ->
			State;
		
		ListenerPID	->
			% ready_listener will be still registered:
			SentState = class_Actor:send_actor_message( ListenerPID,
				notifyReady, State ),
			?setAttribute(SentState,ready_listener,none)	
 
	end,

	% The 'act' method of this actor will be able to use these random values
	% directly this tick:
	?wooper_return_state_only( ?setAttributes( ReadyState, [
		% All random lists of a given stochastic actor are requested at the
		% same time, thus they all should be filled also at the same time 
		% (thus 'completed' may be set more than once):
		{initialization_status,completed}, 
		% These newer values are to be put last:
		{ RequestIdentifier, 
			{ CurrentRandomList ++ Values,RandomType,MinSize } } ] ) ).



% Called by the random manager, in answer to a getExponentialValues call.
% (actor oneway)
setExponentialValues(State,[RequestIdentifier,Values],_SenderPid) ->

	?debug([ io_lib:format( 
		"setExponentialValues called for list ~w with values ~w.", 
		[ RequestIdentifier, Values ] ) ]),

	% Ex: {[],{exponential,80},4}
	{CurrentRandomList,RandomType,MinSize} = ?getAttr(RequestIdentifier),
	
	% Now that random lists are being filled, notify any ready listener:
	ReadyState = case ?getAttr(ready_listener) of
	
		none ->
			State;
		
		ListenerPID	->
			% ready_listener will be still registered:
			SentState = class_Actor:send_actor_message( ListenerPID,
				notifyReady, State ),
			?setAttribute(SentState,ready_listener,none)	
 
	end,

	% The 'act' method of this actor will be able to use these random values
	% directly this tick:
	?wooper_return_state_only( ?setAttributes( ReadyState, [
		% All random lists of a given stochastic actor are requested at the
		% same time, thus they all should be filled also at the same time 
		% (thus 'completed' may be set more than once):
		{initialization_status,completed}, 
		% These newer values are to be put last:
		{ RequestIdentifier, 
			{ CurrentRandomList ++ Values,RandomType,MinSize } } ] ) ).


% Called by the random manager, in answer to a
% getPositiveIntegerExponentialValues call.
% (actor oneway)
setPositiveIntegerExponentialValues( State, [RequestIdentifier,Values],
		_SenderPid ) ->

	?debug([ io_lib:format( "setPositiveIntegerExponentialValues "
		"called for list ~w with values ~w.", [RequestIdentifier,Values] ) ]),

	% Ex: {[],{positive_integer_exponential,80},4}
	{CurrentRandomList,RandomType,MinSize} = ?getAttr(RequestIdentifier),


	% Now that random lists are being filled, notify any ready listener:
	ReadyState = case ?getAttr(ready_listener) of
	
		none ->
			State;
		
		ListenerPID	->
			% ready_listener will be still registered:
			SentState = class_Actor:send_actor_message( ListenerPID,
				notifyReady, State ),
			?setAttribute(SentState,ready_listener,none)	
 
	end,
	
	% The 'act' method of this actor will be able to use these random values
	% directly this tick:
	?wooper_return_state_only( ?setAttributes( ReadyState, [
		% All random lists of a given stochastic actor are requested at the
		% same time, thus they all should be filled also at the same time 
		% (thus 'completed' may be set more than once):
		{initialization_status,completed}, 
		% These newer values are to be put last:
		{ RequestIdentifier, 
			{ CurrentRandomList ++ Values,RandomType,MinSize } } ] ) ).



% Called by the random manager, in answer to a getGaussianValues call.
% (actor oneway)
setGaussianValues(State,[RequestIdentifier,Values],_SenderPid) ->

	?debug([ io_lib:format( 
		"setGaussianValues called for list ~w with values ~w.",
		[ RequestIdentifier, Values ] ) ]),

	% Ex: {[],{gaussian,{70,5}},4}
	{CurrentRandomList,RandomType,MinSize} = ?getAttr(RequestIdentifier),
	
	% Now that random lists are being filled, notify any ready listener:
	ReadyState = case ?getAttr(ready_listener) of
	
		none ->
			State;
		
		ListenerPID	->
			% ready_listener will be still registered:
			SentState = class_Actor:send_actor_message( ListenerPID,
				notifyReady, State ),
			?setAttribute(SentState,ready_listener,none)	
 
	end,

	% The 'act' method of this actor will be able to use these random values
	% directly this tick:
	?wooper_return_state_only( ?setAttributes( ReadyState, [
		% All random lists of a given stochastic actor are requested at the
		% same time, thus they all should be filled also at the same time 
		% (thus 'completed' may be set more than once):
		{initialization_status,completed}, 
		% These newer values are to be put last:
		{ RequestIdentifier, 
			{ CurrentRandomList ++ Values,RandomType,MinSize } } ] ) ).



% Called by the random manager, in answer to a getPositiveIntegerGaussianValues
% call.
% (actor oneway)
setPositiveIntegerGaussianValues( State, [RequestIdentifier,Values], 
		_SenderPid) ->

	?debug([ io_lib:format( 
		"setPositiveIntegerGaussianValues called for list ~w with values ~w.",
		[ RequestIdentifier, Values ] ) ]),

	% Ex: {[],{positive_integer_gaussian,{70,5}},4}
	{CurrentRandomList,RandomType,MinSize} = ?getAttr(RequestIdentifier),
	
	% Now that random lists are being filled, notify any ready listener:
	ReadyState = case ?getAttr(ready_listener) of
	
		none ->
			State;
		
		ListenerPID	->
			% ready_listener will be still registered:
			SentState = class_Actor:send_actor_message( ListenerPID,
				notifyReady, State ),
			?setAttribute(SentState,ready_listener,none)	
 
	end,

	% The 'act' method of this actor will be able to use these random values
	% directly this tick:
	?wooper_return_state_only( ?setAttributes( ReadyState, [
		% All random lists of a given stochastic actor are requested at the
		% same time, thus they all should be filled also at the same time 
		% (thus 'completed' may be set more than once):
		{initialization_status,completed}, 
		% These newer values are to be put last:
		{ RequestIdentifier, 
			{ CurrentRandomList ++ Values,RandomType,MinSize } } ] ) ).



% 'act' method not redefined here, expected to be in child classes.
% First action of the 'act' method should be to request list refill,
% so that initialization_status can progress to the 'completed' state. 
% See class_TestStochasticActor.erl 


% Returns a new random value, as described in specified attribute, whose
% attribute value must be a triplet {AvailableList,RandomType,MinSize} with:
%  - AvailableList: list of locally available random values (as returned by the
% random manager), from which requested values will be popped one by one
%  - RandomType is a tuple describing the type of desired randomness, which 
% can be among:
%     * {uniform,N} for uniform laws
%     * {exponential,Lambda} for exponential laws
%     * {gaussian,Mu,Sigma} for gaussian laws
%  - MinSize is the minimum size of the list of random values, which should be
% greater than twice the maximum number of needed random values during one tick
% (the factor 2 is due to the fact that when an actor requests random values to
% the Random Manager, two ticks will elapse until it can use these values).
% Returns a pair made of an updated state and the popped random value:
% {UpdatedState,RandomValue}.
get_random_value_from(AttributeName,State) ->
	
	% There should be at least one element remaining, as refilled on purpose:
	% (otherwise the set MinSize was not high enough)
	
	% Was: '{[H|T],RandomType,MinSize} = ?getAttr(AttributeName),' but the
	% interpretation of a badmatch resulting from an exhausted list was too
	% painful, thus an explicit test is performed:
	case ?getAttr(AttributeName) of
			
		{[H|T],RandomType,MinSize} ->
			NewState = case length(T) of
	
				% Strict equality, not lower or equal: only one request per 
				% refill need.
				MinSize ->
					% Specifying attribute name allows to request multiple
					% random series independently:
					% (requesting 2*MinSize to limit the number of refills)
					% Note that the minimum cache size and the size of a 
					% refill do not have anything in common.
					?debug([ io_lib:format( "get_random_value_from: "
						"requesting a refill of ~w values for list ~w.", 
						[ 2*MinSize, AttributeName ] ) ]),
			
					% Returns an updated state:
					request_refill_of_random_buffer( AttributeName,	RandomType,
						get_requested_count_for(MinSize), State );
		
				_ ->	
					% Still enough random values:
					State
			
			end,
			% Pops the head and returns it:
			{ ?setAttribute(NewState,AttributeName,{T,RandomType,MinSize}), H };
	
		{[],RandomType,MinSize} ->
			?error([ io_lib:format( 
				"get_random_value_from: random list ~s (type: ~w) exhausted, "
				 "its minimal size (~B) should be increased.", 
				 [AttributeName,RandomType,MinSize] ) ]),
			exit(stochastic_actor_exhausted_random_list)
			
	end. 
	

		
% Updates initial state according to specified list of random lists.
prepare_random_lists( ListOfRandomLists, ActorState ) ->
	build_random_lists( ListOfRandomLists, 
		?setAttributes( ActorState, [ {random_lists,[]},
			{initialization_status,blank} ] ) ).


% Updates initial state according to specified random lists.
build_random_lists( [], State ) ->
	State;

% Example of random list: { test_first_uniform, {uniform, 5}, 10} }
build_random_lists( [{ListName,RandomType,MaxDrawnPerTick}|T], State ) ->

	% Each random list starts empty:
	% (Triplet: list of values, random type tuple, minimum cached values)
	%
	% 4*MaxDrawnPerTick+10: high threshold to ensure lists will be refilled on
	% time, knowing that two ticks will elapse until their are replenished,
	% and that, depending on consumption profile (low threshold of list length 
	% may be hit at first consumption in tick T) and on the reordering of
	% incoming calls (refill may be taken into account after all the 
	% consumption of tick T+2), up to 3*MaxDrawnPerTick values may be 
	% consumed <= 4*MaxDrawnPerTick+10 = MinSize.
	ListState = ?setAttribute( State, ListName,	{[],RandomType,
		4*MaxDrawnPerTick+10} ),
	
	build_random_lists( T, 
		?appendToAttribute( ListState, random_lists, ListName ) ).
		

		
% Triggers the initialization of all known random buffers, requesting the
% random manager to help filling them.
initialize_random_buffers(State) ->
	initialize_random_buffers(?getAttr(random_lists),State).
	
	
% Triggers the initialization of the specified list of random buffers.
initialize_random_buffers( [], State ) ->
	State;
 		
initialize_random_buffers( [RandomListName|T], State ) ->

	?debug([ io_lib:format( "Initializing random list ~w.", [ RandomListName ] )
		]),
	% Checking by pattern matching (for the empty list):
	{[],RandomType,MinSize} = ?getAttr(RandomListName),
	
	% Request a rather large number of samples:
	initialize_random_buffers( T, request_refill_of_random_buffer(
		RandomListName, RandomType, get_requested_count_for(MinSize), State ) ).  


% Determines how many values should be requested when hitting the low threshold
% of a random list. 
% Note: this number should not be lower than MinSize, otherwise the actor may
% exhaust its cached values. 
get_requested_count_for(MinSize) ->
	2*MinSize.
	
	
% Requests the random manager to replenish the buffer of a given distribution.
% Returns an updated state.
request_refill_of_random_buffer( AttributeName, {uniform,N}, Count, State ) ->
	% Requests Count new values:
	class_Actor:send_actor_message( ?getAttr(random_manager_pid),
		{ getUniformValues, [N,Count,AttributeName] }, State );

request_refill_of_random_buffer( AttributeName, {exponential,Lambda}, 
		Count, State ) ->
	% Requests Count new values:
	class_Actor:send_actor_message( ?getAttr(random_manager_pid),
		{ getExponentialValues, [Lambda,Count,AttributeName] }, State );

request_refill_of_random_buffer( AttributeName, 
		{positive_integer_exponential,Lambda}, Count, State ) ->
	% Requests Count new values:
	class_Actor:send_actor_message( ?getAttr(random_manager_pid),
		{ getPositiveIntegerExponentialValues, 
			[Lambda,Count,AttributeName] }, State );

request_refill_of_random_buffer( AttributeName, {gaussian,Mu,Sigma}, 
		Count, State ) ->
	% Requests Count new values:
	class_Actor:send_actor_message( ?getAttr(random_manager_pid),
		{ getGaussianValues, [Mu,Sigma,Count,AttributeName] }, State );

request_refill_of_random_buffer( AttributeName,
		{positive_integer_gaussian,Mu,Sigma}, Count, State ) ->
	% Requests Count new values:
	class_Actor:send_actor_message( ?getAttr(random_manager_pid),
		{ getPositiveIntegerGaussianValues, [Mu,Sigma,Count,AttributeName] },
		State ).

