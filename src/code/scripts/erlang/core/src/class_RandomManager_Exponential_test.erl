% Unit test for the RandomManager class implementation, regarding the
% exponential distribution.
% See the class_RandomManager.erl module.
-module(class_RandomManager_Exponential_test).


-define(Tested_modules,[class_RandomManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).



show_exponential(RandomManagerPid,Lambda) ->

	?test_debug([ io_lib:format( 
		"Requesting an exponential random value with lambda = ~w.", 
		[ Lambda ] ) ]),
				
	RandomManagerPid ! { getExponentialValue, [Lambda], self() },

	receive
	
		{wooper_result,{exponential_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received exponential random value: ~w.", [ Value ] ) ])	
						
	end.



draw_exponential_values(0,Table,_,_) ->
	Table;
	
draw_exponential_values(Count,Table,Lambda,RandomManagerPid) ->
	RandomManagerPid ! { getPositiveIntegerExponentialValue, [Lambda], self() },

	% Wanting a random value in ]1,?table_span]:
	receive
	
		{wooper_result,{positive_integer_exponential_value,Value}} 
				when Value > ?table_span ; Value =< 0 ->
			draw_exponential_values(Count,Table,Lambda,RandomManagerPid);

		{wooper_result,{positive_integer_exponential_value,Value}} ->
			NewCount = element(Value,Table) + 1,
			draw_exponential_values(Count-1, 
				setelement( Value, Table, NewCount ), Lambda ,RandomManagerPid)
						
	end.
	
		

	
% At index V there is the number of times V has been drawn.
make_table(Size) ->
	erlang:make_tuple(Size,0).

	
send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, ProbePid ) ->
	send_tables(FirstTable,SecondTable,ThirdTable,FourthTable,ProbePid,1).
	
	
send_tables(_,_,_,_,_,?table_span+1) ->
	ok;
	
send_tables( FirstTable, SecondTable, ThirdTable, FourthTable,
		ProbePid, Count ) ->
	ProbePid ! { setData, [ Count, { element(Count,FirstTable),
		element(Count,SecondTable),  element(Count,ThirdTable),
		element(Count,FourthTable) } ] },
	send_tables(FirstTable,SecondTable,ThirdTable,FourthTable,ProbePid,Count+1).
	 


compute_mean(Table) ->
	List = tuple_to_list(Table),
	% Multiply the number of draws by the drawn value:
	% (hope the sum is not zero ! Starting at index 1)
	compute_mean( List, 1, 0 ) / compute_sum(List,0).
	

% Counts the number of draws.	
compute_sum([],Count) ->	
	Count;
	
compute_sum([H|T],Count) ->	
	compute_sum(T,Count+H).	
	

% Counts the sum of draws.	
compute_mean([],_Index,Acc) ->	
	Acc;
	
compute_mean([H|T],Index,Acc) ->	
	compute_mean(T,Index+1,Acc+H*Index).
	
		
	
test_exponential_random(RandomManagerPid, Lambda) ->

	?test_info([ "Requesting exponential random values." ]),
	
	show_exponential(RandomManagerPid,Lambda),
	show_exponential(RandomManagerPid,Lambda),
	show_exponential(RandomManagerPid,Lambda),
	show_exponential(RandomManagerPid,Lambda),
	show_exponential(RandomManagerPid,Lambda),

	?test_info([ "Computing and displaying the full actual "
		"exponential distribution." ]),

	Values = make_table(?table_span),
		
	FirstExponentialTable = draw_exponential_values( 500, Values, Lambda,
		RandomManagerPid ),
			
	SecondExponentialTable =
		draw_exponential_values( 5000-500, FirstExponentialTable, Lambda,
			RandomManagerPid),

	ThirdExponentialTable =
		draw_exponential_values(50000-5000, SecondExponentialTable, Lambda,
			RandomManagerPid),
		
	FourthExponentialTable =
		draw_exponential_values( 500000-50000, ThirdExponentialTable, 
			Lambda, RandomManagerPid ),

	Mean = compute_mean(FourthExponentialTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"exponential distribution is ~w.", [ Mean ] ) ]),

	MyExponentialProbe = class_Probe:new( "Exponential probe",
		{ "After 500 draws","After 5000 draws","After 50000 draws",
		"After 500000 draws" },

		io_lib:format( "Test of the generation of exponential random "
			"distributions with lambda = ~w.", [ Lambda ] ),
		io_lib:format( "Drawn values (mean value is ~w, "
			"expected to be ~w)", [ Mean, 1/Lambda ] ), 
		"Number of times a value has been drawn" ),		
		
	send_tables( FirstExponentialTable, SecondExponentialTable,
		ThirdExponentialTable, FourthExponentialTable, MyExponentialProbe),

	?test_info([ "Requesting the generation of exponential probe report." ]),

	?generateReportForProbe(MyExponentialProbe),
	
	MyExponentialProbe ! delete.




	
% Run the tests, no prior RandomManager expected to be alive.
run() ->

	?test_start,
	
	?test_info([ "Creating a new TimeManager." ]),
	class_TimeManager:create(),

	?test_info([ "Creating a new RandomManager." ]),
	class_RandomManager:create(),

	RandomManagerPid = case basic_utils:wait_for_global_registration_of( 
			?random_manager_name ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( 
				io_lib:format( "Unable to find target RandomManager: ~w",
				[ Error ] ) )
	
	end,	

	Lambda = 0.1,
	test_exponential_random(RandomManagerPid,Lambda),
		
	?test_info([ "Removing random manager." ]),
	class_RandomManager:remove(),

	?test_info([ "Removing time manager." ]),
	class_TimeManager:remove(),

	?test_stop.

