% Unit tests for the Probe class implementation.
% @see the class_Probe.erl module.
-module(class_Probe_test).


-define(Tested_modules,[class_Probe]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For time_manager_name:
-include("class_TimeManager.hrl").
	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new Probe." ]),
		
	MyProbe = class_Probe:new( "Test probe",
		{"First dimension","Second dimension","Third dimension"},
		"This is a test of the generic probe class",
		"Simulation tick (20 ms)", "Number of events" ),		
	
	?test_info([ "Sending data to the probe." ]),

	MyProbe ! { setData, [1, {1,3,7 } ] },
	MyProbe ! { setData, [2, {2,2,3 } ] },
	MyProbe ! { setData, [3, {3,3,0 } ] },
	MyProbe ! { setData, [4, {4,2,-1} ] },
	MyProbe ! { setData, [5, {4,3,1 } ] },
	MyProbe ! { setData, [6, {5,2,3 } ] },
	MyProbe ! { setData, [7, {4,3,7 } ] },
	
	% Changing the default settings:
	MyProbe ! { setKeyOptions, ["outside right"] },
	
	?test_info([ "Requesting the generation of probe report." ]),

	?generateReportForProbe(MyProbe),
	
	MyProbe ! delete,

	?test_stop.

