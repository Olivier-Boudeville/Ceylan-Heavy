% Unit tests for the TimeManager class implementations, in batch mode.
% See the class_TimeManager.erl module.
% Note This test must be run from a networked node (using -name).
% See class_TimeManager.erl
-module(class_TimeManager_batch_test).


-define(Tested_modules,[class_TimeManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For time_manager_name:
-include("class_TimeManager.hrl").


		

% Helper function to flush tick notifications from the test mailbox.
flush_tick_notifications() ->

	?test_info([ "Getting rid of any pending top in mailbox." ]),

	receive
	
		{ beginTick, _Tick } ->
			flush_tick_notifications()
	
	after 0 ->
		ok
	
	end.
	
	

% Run the tests in batch mode, no prior TimeManager expected to be alive.
run() ->

	?test_start,
	
	% No external time manager used anymore:
	% Time manager is expected to be on this node on the same computer:
	%TargetNode = list_to_atom( "ceylan_test@" ++ net_adm:localhost() ),
	
	% Let's connect this node to the time manager one:
	%case net_adm:ping(TargetNode) of
	%
	%	pong -> 
	%		ok;
	%		
	%	_ ->
	%		testFailed( io_lib:format( 
	%			"Unable to create a link to this target node ~w "
	%			"(check host resolution)", [ TargetNode ] ) )
	%				
	%end,

	?test_info([ "Creating a non-interactive TimeManager." ]),
	class_TimeManager:create(false),

	TimeManagerPid = case utils:wait_for_global_registration_of( 
			?time_manager_name ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( io_lib:format( "Unable to find target TimeManager: ~w",
				[ Error ] ) )
	
	end,

	?test_info([ "Starting it." ]),
	class_TimeManager:start(),

	?test_info([ "Requesting textual timings (first)." ]),
		
	% Apparently a globally registered name cannot be used with '!', thus
	% Pid must be used, neither ?time_manager_name nor ceylan_time_manager
	% will work: 
	TimeManagerPid ! { getTextualTimings,[],self() },

	receive
	
		{wooper_result,FirstTimingString} when is_list(FirstTimingString) ->
			?test_info([ io_lib:format( "Received first time: ~s.", 
				[ FirstTimingString ] ) ] )	
						
	end,
	
	WaitedSeconds = 2,

	?test_info([ io_lib:format("Sleeping for ~w seconds.",[WaitedSeconds]) ]),
	timer:sleep( WaitedSeconds * 1000 ),
	?test_info([ "End of sleep." ]),

	?test_info([ "Stopping manager." ]),
	class_TimeManager:stop(),

	% Assignment:
	Tick = class_TimeManager:getSimulationTick(),

	?test_info([ io_lib:format( "Current tick is now: ~w.", [ Tick ] ) ]),	
	
	% Pattern matching (same tick, as stopped):
	Tick = class_TimeManager:getSimulationTick(),

	TimeManagerPid ! { getSimulationDate,[],self() },

	receive

		{wooper_result,Date} when is_tuple(Date) ->
			?test_info([ io_lib:format( "Received structured date: ~w.", 
				[ Date ] ) ])	

	end,

	?test_info([ "Testing unsubscription." ]),
	class_TimeManager:start(),

	% Tries to unsubscribe whereas not subscribed:
	class_TimeManager:getManager() ! { unsubscribe, [], self() },
	receive

		{wooper_result,not_already_time_subscribed} ->
			?test_info([ "Test instance unsubscription, "
				"whereas not subscribed, correctly managed." ]);

		% Prevent any shutting down of the trace supervisor from interfering: 
		{wooper_result,AnAtom} when is_atom(AnAtom) andalso 
				AnAtom =/= monitor_ok ->		
			testFailed( io_lib:format( "Unable to unsubscribe correctly from "
				"TimeManager whereas not subscribed: ~w", 
					[ AnAtom ] ) )

	end,

	% First-time subscription (pattern-matching):
	time_subscribed = class_TimeManager:subscribe(),

	%class_TimeManager:getManager() ! { subscribe, [], self() },
	%receive
	%
	%	{wooper_result,time_subscribed} ->
	%		io:format( ?Prefix 
	%			"Test instance subscribed as a time listener.~n" );	
	%	
	%	FirstUnexpected ->
	%		testFailed( io_lib:format( "Unable to subscribe to TimeManager: ~w",
	%			[ FirstUnexpected ] ) )
	%	
	%end,
	
	% Subscribe twice:
	class_TimeManager:getManager() ! { subscribe, [], self() },
	receive
	
		{wooper_result,already_time_subscribed} ->
			?test_info([ "Test instance subscribed twice "
				"as a time listener, it has been correctly managed." ]);	

		% Prevent any shutting down of the trace supervisor from interfering: 
		{wooper_result,OtherAtom} when is_atom(OtherAtom) andalso 
				OtherAtom =/= monitor_ok ->	
			testFailed( io_lib:format( 
				"Unable to subscribe correctly twice to TimeManager: ~w.", 
					[ OtherAtom ] ) )

	end,

	class_TimeManager:stop(),

	flush_tick_notifications(),
	
	class_TimeManager:start(),
	
	?test_info([ "Subscribed, waiting for a first top." ]),
	receive
	
		{beginTick,FirstTick} ->
			?test_info([ io_lib:format( "Received first top: ~B.", 
				[ FirstTick ] ) ]),	
			% One can try FirstTick-1 or FirstTick+1 to check that errors are
			% detected:
			TimeManagerPid ! { done, {FirstTick,self()} },
			?test_info([ "First top action acknowledged to the manager "
				"by the test, with a 'done' message." ])
			
	end,

	?test_info([ "Waiting for a second top." ]),
	receive
	
		{beginTick,SecondTick} ->
			?test_info([ io_lib:format( "Received second top: ~B.", 
				[ SecondTick ] ) ]),	
			TimeManagerPid ! { done, {SecondTick,self()} },
			?test_info([ "Second top action acknowledged to the manager "
				"by the test, with a 'done' message." ])
	
	end,

	?test_info([ "Waiting for a third top." ]),
	receive

		{beginTick,ThirdTick} ->
			?test_info([ io_lib:format( "Received third top: ~B.", 
				[ ThirdTick ] ) ]),	
			TimeManagerPid ! { terminated, {ThirdTick,self()} },
			?test_info([ "Third top action acknowledged to the manager "
				"by the test, with a 'terminated' message." ])

	end,


	% Unsubscribe:
	%class_TimeManager:getManager() ! { unsubscribe, [], self() },
	%receive
	%
	%	{wooper_result,time_unsubscribed} ->
	%		 ?test_info([
	%			"Test instance unsubscription correctly managed." ]);	
	%	
	%	ThirdUnexpected ->
	%		testFailed( io_lib:format( 
	%			"Unable to unsubscribe correctly from TimeManager: ~w", 
	%			[ ThirdUnexpected ] ) )
	%
	%end,

	?test_info([ "Trying to unsubscribe twice "
		"(already unsubscribed, as actor has been terminated)" ]),

	% Wait a bit so that this test process gets effectively unsubscribed from
	% the  time manager, otherwise next unsubscribe call would return
	% 'time_unsubscribed' instead of 'not_already_time_subscribed':
	timer:sleep( 1000 ),

	not_already_time_subscribed = class_TimeManager:unsubscribe(),


	?test_info([ "Trying to unsubscribe again." ]),
	class_TimeManager:getManager() ! { unsubscribe, [], self() },
	receive
	
		{wooper_result,not_already_time_subscribed} ->
			?test_info([ 
				"Test instance double unsubscription correctly managed." ]);	
		
		% Prevent any shutting down of the trace supervisor from interfering: 
		{wooper_result,AnotherAtom} when is_atom(AnotherAtom) andalso 
				AnotherAtom =/= monitor_ok ->	
			testFailed( io_lib:format( "Unable to unsubscribe correctly from "
				"TimeManager whereas not subscribed: ~w", 
					[ AnotherAtom ] ) )
	
	end,

	?test_info([ "Stopping manager." ]),
	class_TimeManager:stop(),
	
	?test_info([ "Removing manager." ]),
	class_TimeManager:remove(),

	?test_stop.

