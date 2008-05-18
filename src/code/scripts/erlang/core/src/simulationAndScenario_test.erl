% Integration test between simulation, scenario and time managers.
% See the class_SimulationManager and class_ScenarioManager tested
% modules.
-module(simulationAndScenario_test).


-define(Tested_modules,[class_SimulationManager,class_ScenarioManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
run() ->

	?test_start,

	?test_info([ "Creating a SimulationManager." ]),
	% 1 Hz, non-interactive:
	SimulationManagerPid = class_SimulationManager:synchronous_new_link( 
		1, false, fixed_length_scenario, self() ),

	receive
	
		simulation_ready_to_run ->
			?test_info([ "Simulation ready to run." ])
				
	end,

	?test_info([ "Starting simulation manager." ]),
	SimulationManagerPid ! start,
	
	receive
	
		{simulation_ended,EndTick} ->
			?test_info([ io_lib:format( "Simulation ended at tick #~B.",
				[EndTick] ) ])
				
	end,
		
	?test_info([ "Simulation Manager will be stopped by Scenario Manager." ]),
	
	?test_info([ "Deleting Simulation manager." ]),
	SimulationManagerPid ! delete,
	
	?test_stop.
	
