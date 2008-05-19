% Unit tests for the Describable class implementation.
% See the class_Describable module.
-module(class_Describable_test).


-define(Tested_modules,[class_Describable]).


% For all facilities common to all tests:
-include("test_constructs.hrl").




% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Describable." ]),
	
	Description = "King Of Brittain",
		
	MyDescribable = class_Describable:new_link( Description ),
	
	
	MyDescribable ! {getDescription,[],self()},
	receive
	
		{wooper_result,Description} ->
			?test_info([ "Correct description returned." ])
			
	end,

	
	NewDescription = "King of the United Kingdom",
	
	MyDescribable ! {setDescription,[NewDescription]},

	MyDescribable ! {getDescription,[],self()},
	receive
	
		{wooper_result,NewDescription} ->
			?test_info([ "Correct new description returned." ])
			
	end,
	
	MyDescribable ! delete,

	?test_stop.

