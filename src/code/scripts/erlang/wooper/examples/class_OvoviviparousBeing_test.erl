% Unit tests for the OvoviviparousBeing class implementation.
% See the class_OvoviviparousBeing.erl tested module.

-module(class_OvoviviparousBeing_test).

-export([run/0]).

-define(Tested_module,class_OvoviviparousBeing).

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
		[ class_OvoviviparousBeing:is_wooper_debug() ] ),	
	io:format( ?Prefix "Statically, class name is ~s, superclasses are ~w.~n", 
		[
			class_OvoviviparousBeing:get_class_name(), 
			class_OvoviviparousBeing:get_superclasses() ] ),
	MyV = class_OvoviviparousBeing:new(),
	MyV ! {get_class_name,[],self()},
	receive
	
		{result,class_OvoviviparousBeing} ->
			io:format( ?Prefix 
				"After constructor, get_class_name returned "
				"'class_OvoviviparousBeing' as expected.~n");
				
		{result,UnexpectedClass} -> 
			testFailed( io_lib:format( "wrong class : ~p",
				[ UnexpectedClass ] ) )
			
	end,
	MyV ! {get_superclasses,[],self()},
	receive
	
		{result, []} ->
			io:format( ?Prefix 
				"After constructor, get_superclasses returned [] "
				"as expected.~n");

		{result,UnexpectedSuperclasses} -> 
			testFailed( io_lib:format( "wrong superclasses : ~p", 
				[ UnexpectedSuperclasses ] ) )
		
	end,
	MyV ! {getMeanEggsCount,[],self()},
	receive
	
		{result,1000} ->
			io:format( ?Prefix 
				"After constructor, getMeanEggsCount returned 4 "
				"as expected.~n");

		{result,UnexpectedMeanCount} -> 
			testFailed( io_lib:format( "wrong mean egg count : ~p", 
				[ UnexpectedMeanCount ] ) )
	
		
	end,
	MyV ! {getEggsLaidCount,[],self()},
	receive
	
		{result,0} ->
			io:format( ?Prefix 
				"After constructor, getEggsLaidCount returned 0 "
				"as expected.~n");
	
		{result,UnexpectedFirstCount} -> 
			testFailed( io_lib:format( "wrong first egg count : ~p", 
				[ UnexpectedFirstCount ] ) )
			
	end,
	MyV ! {layEggs,747},
	MyV ! {getEggsLaidCount,[],self()},
	receive
	
		 {result,747}->
			io:format(?Prefix 
				"After giveBirth, getEggsLaidCount returned 747 "
				"as expected.~n");
	
		{result,UnexpectedSecondCount} -> 
			testFailed( io_lib:format( "wrong second egg count : ~p", 
				[ UnexpectedSecondCount ] ) )
				
	end,		
	case class_OvoviviparousBeing:is_wooper_debug() of 
		true ->
			MyV ! { wooper_get_instance_description,[], self() },
			receive
			
				{result,InspectString} ->
					io:format( "~s~n", [ InspectString ] )
			end ;		
		false ->
			ok	
	end,				
	io:format( ?Prefix "End of test for module ~s.~n", [ ?Tested_module ] ),
	testFinished().

