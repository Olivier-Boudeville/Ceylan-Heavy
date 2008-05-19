% Unit tests for the Locatable class implementation.
% See the class_Locatable module.
-module(class_Locatable_test).


-define(Tested_modules,[class_Locatable]).


% For all facilities common to all tests:
-include("test_constructs.hrl").

% For location:
-include("space.hrl").
	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Locatable." ]),
	
	MyLocation = #location{ x = 1, y = 2, z = 3 },
		
	MyLocatable = class_Locatable:new_link( MyLocation ),		
	
	MyLocatable ! {getLocation,[],self()},
	
	receive
	
		{wooper_result,MyLocation} ->
				?test_info([ "getLocation succeeded." ])

	end,
	
	Origin = space:origin(),
	
	?test_info([ io_lib:format("Origin is ~w.", [ Origin ]) ]),
	
	MyLocatable ! {setLocation,Origin},
	
	MyLocatable ! {getLocation,[],self()},

	receive
	
		{wooper_result,Origin} ->
				?test_info([ "setLocation succeeded." ]);

		{wooper_result,Other} ->
				?test_error([ 
					io_lib:format("setLocation failed: ~w.", [ Other ]) ])

	end,
	
	Abscissa = 7,
	MyLocatable ! {setAbscissa,Abscissa},
	MyLocatable ! {getAbscissa,[],self()},
	
	receive
	
		{wooper_result,Abscissa} ->
				?test_info([ "setAbscissa and getAbscissa succeeded." ])

	end,

	Ordinate = 17,
	Altitude = 22,

	MyLocatable ! {setOrdinate,Ordinate},
	MyLocatable ! {setAltitude,Altitude},
	
	FinalLocation = #location{x=Abscissa,y=Ordinate,z=Altitude},

	MyLocatable ! {getLocation,[],self()},
	receive
	
		{wooper_result,FinalLocation} ->
				?test_info([ "set/get for abscissa, ordinate and altitude "
					"succeeded." ])

	end,
	
	MyLocatable ! delete,

	?test_stop.

