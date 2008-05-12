% Unit tests for the interaction between the random manager and stochastic
% actors.
% See the class_RandomManager.erl and class_TestStochasticActor.erl tested
% modules.
-module(randomManagerAndStochasticActorPair_test).


-define(Tested_modules,[class_RandomManager,class_TestStochasticActor]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For time_manager_name:
-include("class_TimeManager.hrl").


% Runs the test.
run() ->

	?test_start,

	?test_info([ "Creating a non-interactive TimeManager." ]),
	class_TimeManager:create(false),

	?test_info([ "Creating the RandomManager." ]),
	class_RandomManager:create(),
	
	% The random lists the test actor will rely on:
	% ({ RandomListName, RandomSettings, MinCount })
	RandomLists = [ 
		{ my_first_uniform,  {uniform,     5},10}, 
		{ my_second_uniform, {uniform,   100}, 4},
		{ my_gaussian,       {gaussian, 50,2}, 5},
		{ my_exponential,    {exponential,80}, 5}
	],
	 
	% Creates an actor that will automatically subscribe itself to the manager
	% and that will terminate on specified tick:
	Cartman = class_TestStochasticActor:synchronous_new( "Cartman", 
		RandomLists, 20 ),
		
	
	TimeManagerPid = case utils:wait_for_global_registration_of( 
			?time_manager_name ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( io_lib:format( "Unable to find target TimeManager: ~w",
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

	% Let some time elapse so that stochastic actors can live their life:
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
	
	?test_info([ "Deleting stochastic actor." ]),
	Cartman ! delete,
	
	?test_info([ "Stopping manager." ]),
	class_TimeManager:stop(),
	
	?test_info([ "Removing manager." ]),
	class_TimeManager:remove(),
	
	?test_stop.
	
