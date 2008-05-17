% Unit tests for the RandomManager class implementation, regarding the
% uniform distribution.
% See the class_RandomManager.erl module.
-module(class_RandomManager_Uniform_test).


-define(Tested_modules,[class_RandomManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).


	
show_uniform(RandomManagerPid, UpperBound) ->

	?test_debug([ io_lib:format( 
		"Requesting a uniform random value in [1,~w].", 
		[ UpperBound ] ) ]),
		
	RandomManagerPid ! { getUniformValue,UpperBound,self() },

	receive
	
		{wooper_result,{uniform_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received uniform random value: ~w.", [ Value ] ) ])	
						
	end.
	
	
	
draw_uniform_values(0,Table,_,_) ->
	Table;
	
draw_uniform_values(Count,Table,MaxValue,RandomManagerPid) ->
	RandomManagerPid ! { getUniformValue, [MaxValue], self() },

	% Wanting a random value in ]1,?table_span]:
	receive
	
		{wooper_result,{uniform_value,Value}} ->
			NewCount = element(Value,Table) + 1,
			draw_uniform_values( Count-1, 
				setelement( Value, Table, NewCount ), 
				MaxValue, RandomManagerPid )
						
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
	
	

test_uniform_random(RandomManagerPid,MaxValue) ->
	
	?test_info([ "Requesting uniform random values." ]),
	
	show_uniform(RandomManagerPid, MaxValue),
	show_uniform(RandomManagerPid, MaxValue),
	show_uniform(RandomManagerPid, MaxValue),
	show_uniform(RandomManagerPid, MaxValue),
	show_uniform(RandomManagerPid, MaxValue),

	?test_info([ "Computing and displaying the full actual "
		"uniform distribution." ]),

	Values = make_table(?table_span),
		
	FirstUniformTable = draw_uniform_values( 500, Values, MaxValue, 
		RandomManagerPid ),
			
	SecondUniformTable =
		draw_uniform_values( 5000-500, FirstUniformTable,
			MaxValue, RandomManagerPid),

	ThirdUniformTable =
		draw_uniform_values( 50000-5000, SecondUniformTable, MaxValue,
			RandomManagerPid),
		
	FourthUniformTable =
		draw_uniform_values( 500000-50000, ThirdUniformTable, MaxValue,
			RandomManagerPid ),

	Mean = compute_mean(FourthUniformTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"uniform distribution is ~w.", [ Mean ] ) ]),

	MyUniformProbe = class_Probe:new( "Uniform probe",
		{ "After 500 draws","After 5000 draws","After 50000 draws",
		"After 500000 draws" },
		io_lib:format( "Test of the generation of uniform random distributions "
			"ranging in [1..~B].", [ ?table_span ] ),
		io_lib:format( "Drawn values (mean value is ~w)", [ Mean ] ), 
		"Number of times a value has been drawn" ),		
		
	send_tables( FirstUniformTable, SecondUniformTable,
		ThirdUniformTable, FourthUniformTable, MyUniformProbe),


	?test_info([ "Requesting the generation of uniform probe report." ]),

	?generateReportForProbe(MyUniformProbe),
	
	MyUniformProbe ! delete.
	

	
% Run the tests, no prior RandomManager expected to be alive.
run() ->

	?test_start,
	
	?test_info([ "Creating a new TimeManager." ]),
	class_TimeManager:create(),

	?test_info([ "Creating a new RandomManager." ]),
	class_RandomManager:create(),

	RandomManagerPid = case utils:wait_for_global_registration_of( 
			?random_manager_name ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( 
				io_lib:format( "Unable to find target RandomManager: ~w",
				[ Error ] ) )
	
	end,	

	Max = ?table_span, 
	test_uniform_random(RandomManagerPid,Max),
		
	?test_info([ "Removing random manager." ]),
	class_RandomManager:remove(),

	?test_info([ "Removing time manager." ]),
	class_TimeManager:remove(),

	?test_stop.

