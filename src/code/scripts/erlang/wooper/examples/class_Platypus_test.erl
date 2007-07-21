% Unit tests for the Platypus class implementation.
% See the class_Platypus.erl tested module.

-module(class_Platypus_test).

-export([run/0]).

-define(Tested_module,class_Platypus).

-define(Prefix,"--> ").



% Comment out to be able to use the interpreter after the test :
-define(ExitAfterTest,).

-ifdef(ExitAfterTest).

testFinished() ->
	erlang:halt().
	
-else.

testFinished() ->
	io:format( "(interpreter still running)~n" ),
	test_success.
	
-endif.


testFailed(Reason) ->
	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable list.
	io:format( "~n!!!! Test failed for module ~s, reason : ~s~n~n",
		[ ?Tested_module, Reason ] ),
	erlang:error( "Test failed" ).	
		

run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Debug mode : ~s.~n", 
		[ class_Platypus:is_wooper_debug() ] ),	
	
		
	% General tests.
	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_Platypus:get_class_name(), 
			class_Platypus:get_superclasses() ] ),
	MyP = class_Platypus:new(4,male,brown,black),
	MyP ! {get_class_name,[],self()},
	receive
	
		{result,class_Platypus} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned 'class_Platypus' "
				"as expected.~n");
				
		{result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class : ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyP ! {get_superclasses,[],self()},
	receive
	
		{result, [class_Mammal,class_OvoviviparousBeing]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned "
				"[class_Creature,class_OvoviviparousBeing] as expected.~n");
				
		{result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses : ~p", 
				[ UnexpectedSuperclasses ] ) )
	
	end,
	
	
	% Tests related to Mammals and Creatures.
	
	MyP ! {getAge,[],self()},
	receive
	
		{result,4} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 4 as expected.~n");

		{result,UnexpectedAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedAge ] ) )
		
	end,
	MyP ! {getGender,[],self()},
	receive
	
		{result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");
	
		{result,UnexpectedGender} -> 
			testFailed( io_lib:format( "wrong gender : ~p", 
				[ UnexpectedGender ] ) )
			
	end,
	MyP ! {setAge,5},
	MyP ! {getAge,[],self()},
	receive
	
		 {result,5}->
			io:format(?Prefix 
				"After setAge, getAge returned 5 as expected.~n");
	
		{result,UnexpectedNewAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedNewAge ] ) )
			
	end,	
	MyP ! declareBirthday,
	MyP ! {getAge,[],self()},
	receive
	
		{result,6}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 6 as expected.~n");

		{result,UnexpectedLastAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedLastAge ] ) )
	
	end,	
	MyP ! declareBirthday,
	MyP ! {isHotBlooded,[],self()},
	receive
	
		{result,true}->
			io:format(?Prefix 
				"isHotBlooded returned true as expected.~n");

		{result,UnexpectedBlood} -> 
			testFailed( io_lib:format( "wrong blood type : ~p", 
				[ UnexpectedBlood ] ) )
	
	end,	
	MyP ! {getFurColor,[],self()},
	receive
	
		 {result,brown}->
			io:format(?Prefix 
				"getFurColor returned brown as expected.~n");

		{result,UnexpectedFurColor} -> 
			testFailed( io_lib:format( "wrong fur color : ~p", 
				[ UnexpectedFurColor ] ) )
	
	end,
	
	
	% Tests related to OvoviviparousBeings.
	
	MyP ! {getMeanEggsCount,[],self()},
	receive
	
		{result,2} ->
			io:format( ?Prefix 
				"After constructor, getMeanEggsCount returned 2 "
				"as expected.~n");

		{result,UnexpectedMeanCount} -> 
			testFailed( io_lib:format( "wrong mean egg count : ~p", 
				[ UnexpectedMeanCount ] ) )
	
		
	end,
	MyP ! {getEggsLaidCount,[],self()},
	receive
	
		{result,0} ->
			io:format( ?Prefix 
				"After constructor, getEggsLaidCount returned 0 "
				"as expected.~n");
	
		{result,UnexpectedFirstCount} -> 
			testFailed( io_lib:format( "wrong first egg count : ~p", 
				[ UnexpectedFirstCount ] ) )
			
	end,
	MyP ! {layEggs,1},
	MyP ! {getEggsLaidCount,[],self()},
	receive
	
		 {result,1}->
			io:format(?Prefix 
				"After giveBirth, getEggsLaidCount returned 1 "
				"as expected.~n");
	
		{result,UnexpectedSecondCount} -> 
			testFailed( io_lib:format( "wrong second egg count : ~p", 
				[ UnexpectedSecondCount ] ) )
				
	end,		
	

	% Tests related to Platypuses.
	
	MyP ! {getTeatCount,[],self()},
	receive
	
		 {result,0}->
			io:format(?Prefix 
				"getTeatCount returned 0 as expected.~n");
	
		{result,UnexpectedTeatCount} -> 
			testFailed( io_lib:format( "wrong teat count : ~p", 
				[ UnexpectedTeatCount ] ) )
				
	end,
	
	MyP ! {canEat,weed,self()},
	receive
	
		 {result,true}->
			io:format(?Prefix 
				"This Platypus can eat weed, as expected.~n");
	
		{result,UnexpectedFoodPreference} -> 
			testFailed( io_lib:format( "wrong food preference : ~p", 
				[ UnexpectedFoodPreference ] ) )
				
	end,		
	
	MyP ! {canEat,mammoth,self()},
	receive
	
		 {result,false}->
			io:format(?Prefix 
				"This Platypus cannot eat mammoth, as expected.~n");
	
		{result,UnexpectedOtherFoodPreference} -> 
			testFailed( io_lib:format( "wrong food preference : ~p", 
				[ UnexpectedOtherFoodPreference ] ) )
				
	end,		
	
	MyP ! {getNozzleColor,[],self()},
	receive
	
		 {result,black}->
			io:format(?Prefix 
				"This Platypus has a black nozzle, as expected.~n");
	
		{result,UnexpectedNozzleColor} -> 
			testFailed( io_lib:format( "wrong nozzle color : ~p", 
				[ UnexpectedNozzleColor ] ) )
				
	end,		
	
	case class_Platypus:is_wooper_debug() of 
		true ->
			MyP ! { wooper_get_instance_description,[], self() },
			receive
			
				{result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,				
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

