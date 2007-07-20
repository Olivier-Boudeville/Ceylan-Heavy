% Unit tests for the Mammal class implementation.
% See the class_Mammal.erl tested module.

-module(class_Mammal_test).

-export([run/0]).

-define(Tested_module,class_Mammal).

-define(Prefix,"--> ").


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
				
		_ -> 
			erlang:exit("Test failed.")
			
	end,
	MyM ! {get_superclasses,[],self()},
	receive
	
		{result, [class_Creature]} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned [class_Creature] "
				"as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
	end,
	MyM ! {getAge,[],self()},
	receive
	
		{result,30} ->
			io:format( ?Prefix 
				"After constructor, getAge returned 30 as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
		
	end,
	MyM ! {getGender,[],self()},
	receive
	
		{result,male} ->
			io:format( ?Prefix 
				"After constructor, getGender returned male as expected.~n");
	
		_ -> 
			erlang:exit("Test failed.")
			
	end,
	MyM ! {setAge,5},
	MyM ! {getAge,[],self()},
	receive
	
		 {result,5}->
			io:format(?Prefix 
				"After setAge, getAge returned 5 as expected.~n");
	
		_ -> 
			erlang:exit("Test failed.")
			
	end,	
	MyM ! declareBirthday,
	MyM ! {getAge,[],self()},
	receive
	
		 {result,6}->
			io:format(?Prefix 
				"After declareBirthday, getAge returned 6 as expected.~n");

		_ -> 
			erlang:exit("Test failed.")
	
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

		_ -> 
			erlang:exit("Test failed.")
	
	end,	
	
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	%erlang:halt(),
	test_success.
