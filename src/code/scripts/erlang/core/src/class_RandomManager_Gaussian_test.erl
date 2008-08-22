% Unit tests for the RandomManager class implementation, regarding the
% gaussian distribution.
% See the class_RandomManager.erl module.
-module(class_RandomManager_Gaussian_test).


-define(Tested_modules,[class_RandomManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).



show_gaussian(RandomManagerPid) ->

	?test_debug([ "Requesting a gaussian random value "
		"with default settings (mean = 0, deviation = 1)." ]),
		
	RandomManagerPid ! { getGaussianValue,[],self() },

	receive
	
		{wooper_result,{gaussian_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received gaussian random value: ~w.", [ Value ] ) ])	
						
	end.
	

	
show_gaussian(RandomManagerPid,Mu,Sigma) ->

	?test_debug([ io_lib:format( "Requesting a gaussian random value "
		"with mean = ~w, deviation = ~w.", [ Mu, Sigma ] ) ]),
		
	RandomManagerPid ! { getGaussianValue, [Mu, Sigma], self() },

	receive
	
		{wooper_result,{gaussian_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received gaussian random value: ~w.", [ Value ] ) ])	
						
	end.
	
		
		
draw_gaussian_values(0,Table,_,_,_) ->
	Table;
	
draw_gaussian_values(Count,Table,Mu,Sigma,RandomManagerPid) ->
	RandomManagerPid ! { getPositiveIntegerGaussianValue, [Mu, Sigma], self() },

	% Wanting a random value in ]1,?table_span]:
	receive
	
		{wooper_result,{positive_integer_gaussian_value,Value}} 
				when Value > ?table_span ; Value == 0 ->
			draw_gaussian_values(Count,Table,Mu,Sigma,RandomManagerPid);

		{wooper_result,{positive_integer_gaussian_value,Value}} ->
			NewCount = element(Value,Table) + 1,
			draw_gaussian_values(Count-1, setelement( Value, Table, NewCount ), 
				Mu,Sigma,RandomManagerPid)
						
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
	

	
test_gaussian_random(RandomManagerPid,Mu,Sigma) ->

	?test_info([ "Requesting gaussian random values (first)." ]),

	show_gaussian(RandomManagerPid),
	show_gaussian(RandomManagerPid),
	show_gaussian(RandomManagerPid),
	show_gaussian(RandomManagerPid),
	show_gaussian(RandomManagerPid),

	?test_info([ "Requesting gaussian random values (second)." ]),
	
	show_gaussian(RandomManagerPid,Mu,Sigma),
	show_gaussian(RandomManagerPid,Mu,Sigma),
	show_gaussian(RandomManagerPid,Mu,Sigma),
	show_gaussian(RandomManagerPid,Mu,Sigma),
	show_gaussian(RandomManagerPid,Mu,Sigma),

	?test_info([ "Computing and displaying the full actual "
		"gaussian distribution." ]),

	
	Values = make_table(?table_span),
	
	FirstGaussianTable =
		draw_gaussian_values( 500, Values, Mu, Sigma, RandomManagerPid ),
		
	%io:format( "Gaussian table = ~w~n", [FirstGaussianTable] ),

	SecondGaussianTable =
		draw_gaussian_values( 5000-500, FirstGaussianTable, Mu, Sigma,
			RandomManagerPid),

	ThirdGaussianTable =
		draw_gaussian_values(50000-5000,SecondGaussianTable,Mu,Sigma,
			RandomManagerPid),
		
	FourthGaussianTable =
		draw_gaussian_values( 500000-50000, ThirdGaussianTable, 
			Mu, Sigma, RandomManagerPid ),
		
	Mean = compute_mean(FourthGaussianTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"gaussian distribution is ~w.", [ Mean ] ) ]),

	MyGaussianProbe = class_Probe:new( "Gaussian probe",
		{"After 500 draws","After 5000 draws","After 50000 draws",
		"After 500000 draws" },
		io_lib:format( "Test of a gaussian "
			"distribution with mean mu = ~w and variance sigma = ~w.", 
			[ Mu,Sigma ]), 
		io_lib:format( "Drawn values (mean value is ~w)", [ Mean ] ), 
		"Number of times a value has been drawn" ),	
					
	send_tables( FirstGaussianTable, SecondGaussianTable,
		ThirdGaussianTable, FourthGaussianTable, MyGaussianProbe),

	?test_info([ "Requesting the generation of gaussian probe report." ]),

	?generateReportForProbe(MyGaussianProbe),

	MyGaussianProbe ! delete.



	
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

	Mu = 20, 
	Sigma = 10,
	test_gaussian_random(RandomManagerPid,Mu,Sigma),
		
	?test_info([ "Removing random manager." ]),
	class_RandomManager:remove(),

	?test_info([ "Removing time manager." ]),
	class_TimeManager:remove(),

	?test_stop.

