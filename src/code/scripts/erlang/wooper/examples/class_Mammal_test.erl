% Unit tests for the Mammal class implementation.
% See the class_Mammal.erl tested module.

-module(class_Mammal_test).

-export([run/0]).

-define(Tested_module,class_Mammal).

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
		[ class_Mammal:is_wooper_debug() ] ),	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_Mammal:get_class_name(), 
			class_Mammal:get_superclasses() ] ),
	MyM = class_Mammal:new(30,male,brown),
	MyM ! {get_class_name,[],self()},
	receive
	
		{result,class_Mammal} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned 'class_Mammal' "
				"as expected.~n");
				
		{result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class : ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyM ! {get_superclasses,[],self()},
	receive
	
		{result, [class_Creature]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned [class_Creature] "
				"as expected.~n");

		{result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses : ~p", 
				[ UnexpectedSuperclasses ] ) )
	
	end,
	MyM ! {getAge,[],self()},
	receive
	
		{result,30} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 30 as expected.~n");

		{result,UnexpectedAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedAge ] ) )
		
	end,
	MyM ! {getGender,[],self()},
	receive
	
		{result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");
	
		{result,UnexpectedGender} -> 
			testFailed( io_lib:format( "wrong gender : ~p", 
				[ UnexpectedGender ] ) )
			
	end,
	MyM ! {setAge,5},
	MyM ! {getAge,[],self()},
	receive
	
		 {result,5}->
			io:format(?Prefix 
				"After setAge, getAge returned 5 as expected.~n");
	
		{result,UnexpectedNewAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedNewAge ] ) )
			
	end,	
	MyM ! declareBirthday,
	MyM ! {getAge,[],self()},
	receive
	
		 {result,6}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 6 as expected.~n");

		{result,UnexpectedLastAge} -> 
			testFailed( io_lib:format( "wrong age : ~p", 
				[ UnexpectedLastAge ] ) )
	
	end,	
	MyM ! declareBirthday,
	MyM ! {isHotBlooded,[],self()},
	receive
	
		 {result,true}->
			io:format(?Prefix 
				"isHotBlooded returned true as expected.~n");

		_ -> 
			erlang:exit("Test failed.")				
	
	end,	
	MyM ! {getFurColor,[],self()},
	receive
	
		 {result,brown}->
			io:format(?Prefix 
				"getFurColor returned brown as expected.~n");

		{result,UnexpectedBlood} -> 
			testFailed( io_lib:format( "wrong blood type : ~p", 
				[ UnexpectedBlood ] ) )
	
	end,
	case class_Mammal:is_wooper_debug() of 
		true ->
			MyM ! { wooper_get_instance_description,[], self() },
			receive
			
				{result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,				
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

