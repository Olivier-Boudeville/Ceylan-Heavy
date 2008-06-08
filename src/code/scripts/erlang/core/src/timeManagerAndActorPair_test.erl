% Unit tests for the interaction between the time manager and actors.
% See the class_TimeManager.erl and class_TestActor.erl tested modules.
-module(timeManagerAndActorPair_test).


-define(Tested_modules,[class_TimeManager,class_TestActor]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For time_manager_name:
-include("class_TimeManager.hrl").

	
% Runs the test.
run() ->

	?test_start,
		
	?test_info([ "Creating a non-interactive TimeManager." ]),
	class_TimeManager:create(false),
	
	% Creates an actor that will automatically subscribe itself to the manager
	% and that will terminate on specified tick:
	% No reference kept, as the actor life cycle is managed by the time manager:
	Ernie = class_TestActor:new("Ernie", 5),
	Bart = class_TestActor:new("Bart", 20),
	
	% Cross-reference actors as peers:
	Ernie ! {addPeer,Bart},
	Bart  ! {addPeer,Ernie},
	
	Luke = class_TestActor:new("Luke", 20),
	Ernie ! {addPeer,Luke},
	Bart  ! {addPeer,Luke},
	
	
	TimeManagerPid = case utils:wait_for_global_registration_of( 
			?time_manager_name ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( io_lib:format( "Unable to find target TimeManager ~w:",
				[ Error ] ) )
	
	end,
	
	?test_info([ "Starting time manager." ]),
	class_TimeManager:start(),
	
	
	?test_info([ "Requesting textual timings (first)." ]),
		
	% Apparently a globally registered name cannot be used with '!', thus
	% Pid must be used, neither ?time_manager_name nor ceylan_time_manager
	% will work: 
	TimeManagerPid ! { getTextualTimings,[],self() },

	receive
	
		{wooper_result,FirstTimingString} ->
			?test_info([ io_lib:format( "Received first time: ~s.", 
				[ FirstTimingString ] ) ])	
						
	end,

	% Let some time elapse so that actors can live their life:
	WaitedSeconds = 2,

	?test_info([ io_lib:format("Sleeping for ~w seconds.",[WaitedSeconds]) ]),
	timer:sleep( WaitedSeconds * 1000 ),

	?test_info([ "End of sleep." ]),
	
	?test_info([ "Requesting textual timings (second)." ]),

	TimeManagerPid ! { getTextualTimings,[],self() },

	receive
	
		{wooper_result,SecondTimingString} ->
			?test_info([ io_lib:format( "Received second time: ~s.", 
				[ SecondTimingString ] ) ])	
						
	end,
	
	?test_info([ "Stopping time manager." ]),
	class_TimeManager:stop(),
	
	?test_info([ "Removing time manager." ]),
	class_TimeManager:remove(),
	
	?test_stop.
	
