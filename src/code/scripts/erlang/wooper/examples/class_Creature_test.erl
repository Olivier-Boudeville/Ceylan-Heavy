% Unit tests for the Creature class implementation.
% See the class_Creature.erl tested module.

-module(class_Creature_test).

-export([run/0]).

-define(Tested_module,class_Creature).

-define(Prefix,"--> ").


run() ->
	io:format( ?Prefix "Testing module ~s.~n", [ ?Tested_module ] ),
	io:format( ?Prefix "Debug mode : ~s.~n", 
		[ class_Creature:is_wooper_debug() ] ),	
	io:format( ?Prefix "Class name is ~s, superclasses are ~w.~n", [
		class_Creature:get_class_name(), class_Creature:get_superclasses() ] ),
	MyC = class_Creature:new(30,male),
	MyC ! {getAge,[],self()},
	receive
	
		{result,30} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 30 as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
	end,
	MyC ! {getGender,[],self()},
	receive
	
		{result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
	end,
	MyC ! {setAge,5},
	MyC ! {getAge,[],self()},
	receive
	
		{result,5}->
			io:format(?Prefix 
				"After setAge, getAge returned 5 as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
	end,	
	MyC ! declareBirthday,
	MyC ! {getAge,[],self()},
	receive
	
		 {result,6}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 6 as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
	end,	
	MyC ! declareBirthday,
	
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	%erlang:halt(),
	test_success.
