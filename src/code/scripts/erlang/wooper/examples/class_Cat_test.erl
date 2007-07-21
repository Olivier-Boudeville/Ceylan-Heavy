% Unit tests for the Cat class implementation.
% See the class_Cat.erl tested module.

-module(class_Cat_test).

-export([run/0]).

-define(Tested_module,class_Cat).

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
		[ class_Cat:is_wooper_debug() ] ),	
	
		
	% General tests.
	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_Cat:get_class_name(), 
			class_Cat:get_superclasses() ] ),
	MyC = class_Cat:new(3,female,sand,white),
	MyC ! {get_class_name,[],self()},
	receive
	
		{result,class_Cat} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned 'class_Cat' "
				"as expected.~n");
				
		{result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class : ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyC ! {get_superclasses,[],self()},
	receive
	
		{result, [class_Mammal,class_ViviparousBeing]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned "
				"[class_Creature,class_ViviparousBeing] as expected.~n");
				
		{result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses : ~p", 
				[ UnexpectedSuperclasses ] ) )
	
	end,
	
	
	% Tests related to Mammals and Creatures.
	
	MyC ! {getAge,[],self()},
	receive
	
		{result,3} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 3 as expected.~n");

		{result,UnexpectedAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedAge ] ) )
		
	end,
	MyC ! {getGender,[],self()},
	receive
	
		{result,female} ->
			io:format( ?Prefix 
				"After constructor, getGender returned female as expected.~n");
	
		{result,UnexpectedGender} -> 
			testFailed( io_lib:format( "wrong gender : ~p", 
				[ UnexpectedGender ] ) )
			
	end,
	MyC ! {setAge,5},
	MyC ! {getAge,[],self()},
	receive
	
		 {result,5}->
			io:format(?Prefix 
				"After setAge, getAge returned 5 as expected.~n");
	
		{result,UnexpectedNewAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedNewAge ] ) )
			
	end,	
	MyC ! declareBirthday,
	MyC ! {getAge,[],self()},
	receive
	
		{result,6}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 6 as expected.~n");

		{result,UnexpectedLastAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedLastAge ] ) )
	
	end,	
	MyC ! declareBirthday,
	MyC ! {isHotBlooded,[],self()},
	receive
	
		{result,true}->
			io:format(?Prefix 
				"isHotBlooded returned true as expected.~n");

		{result,UnexpectedBlood} -> 
			testFailed( io_lib:format( "wrong blood type : ~p", 
				[ UnexpectedBlood ] ) )
	
	end,	
	MyC ! {getFurColor,[],self()},
	receive
	
		 {result,sand}->
			io:format(?Prefix 
				"getFurColor returned sand as expected.~n");

		{result,UnexpectedFurColor} -> 
			testFailed( io_lib:format( "wrong fur color : ~p", 
				[ UnexpectedFurColor ] ) )
	
	end,
	
	
	% Tests related to ViviparousBeings.
	
	MyC ! {getMeanChildrenCount,[],self()},
	receive
	
		{result,4} ->
			io:format( ?Prefix 
				"After constructor, getMeanChildrenCount returned 4 "
				"as expected.~n");

		{result,UnexpectedMeanCount} -> 
			testFailed( io_lib:format( "wrong mean children count : ~p", 
				[ UnexpectedMeanCount ] ) )
		
	end,
	MyC ! {getBirthGivenCount,[],self()},
	receive
	
		{result,0} ->
			io:format( ?Prefix 
				"After constructor, getBirthGivenCount returned 0 "
				"as expected.~n");
	
		{result,UnexpectedFirstCount} -> 
			testFailed( io_lib:format( "wrong first children count : ~p", 
				[ UnexpectedFirstCount ] ) )
			
	end,
	MyC ! {giveBirth,5},
	MyC ! {getBirthGivenCount,[],self()},
	receive
	
		 {result,5}->
			io:format(?Prefix 
				"After giveBirth, getBirthGivenCount returned 5 "
				"as expected.~n");
	
		{result,UnexpectedSecondCount} -> 
			testFailed( io_lib:format( "wrong second children count : ~p", 
				[ UnexpectedSecondCount ] ) )
				
	end,		
	
	
	% Tests related to Cats.
	
	MyC ! {getTeatCount,[],self()},
	receive
	
		 {result,6}->
			io:format(?Prefix 
				"getTeatCount returned 6 as expected.~n");
	
		{result,UnexpectedTeatCount} -> 
			testFailed( io_lib:format( "wrong teat count : ~p", 
				[ UnexpectedTeatCount ] ) )
				
	end,		
	
	MyC ! {canEat,soup,self()},
	receive
	
		 {result,true}->
			io:format(?Prefix 
				"This cat can eat soup, as expected.~n");
	
		{result,UnexpectedFoodPreference} -> 
			testFailed( io_lib:format( "wrong food preference : ~p", 
				[ UnexpectedFoodPreference ] ) )
				
	end,		
	
	MyC ! {canEat,tangerine,self()},
	receive
	
		 {result,false}->
			io:format(?Prefix 
				"This cat cannot eat tangerine, as expected.~n");
	
		{result,UnexpectedOtherFoodPreference} -> 
			testFailed( io_lib:format( "wrong food preference : ~p", 
				[ UnexpectedOtherFoodPreference ] ) )
				
	end,		
	
	MyC ! {getWhiskerColor,[],self()},
	receive
	
		 {result,white}->
			io:format(?Prefix 
				"This cat has white whiskers, as expected.~n");
	
		{result,UnexpectedWhiskerColor} -> 
			testFailed( io_lib:format( "wrong whisker color : ~p", 
				[ UnexpectedWhiskerColor ] ) )
				
	end,		
	
	case class_Cat:is_wooper_debug() of 
		true ->
			MyC ! { wooper_get_instance_description,[], self() },
			receive
			
				{result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,				
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().
