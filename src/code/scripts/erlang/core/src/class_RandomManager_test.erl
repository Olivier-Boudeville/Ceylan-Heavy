% Overall unit tests for the RandomManager class implementation.
% See the class_RandomManager.erl module.
% See also the various tests for each supported distribution 
% (class_RandomManager_*_test.erl).
-module(class_RandomManager_test).


-define(Tested_modules,[class_RandomManager]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).



	
show_uniform(RandomManagerPid, UpperBound) ->

	?test_debug([ io_lib:format( 
		"Requesting a uniform random value in [1,~w].", [ UpperBound ] ) ]),
		
	RandomManagerPid ! { getUniformValue, UpperBound, self() },

	receive
	
		{wooper_result,{uniform_value,Value}} ->
			?test_debug([ io_lib:format( "Received uniform random value: ~w.", 
				[ Value ] ) ])	
						
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
	
	
	

show_gaussian(RandomManagerPid) ->

	?test_debug([ "Requesting a gaussian random value "
		"with default settings (mean = 0, deviation = 1)." ]),
		
	RandomManagerPid ! { getGaussianValue,[],self() },

	receive
	
		{wooper_result,{gaussian_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received a gaussian random value: ~w.", [ Value ] ) ])	
						
	end.
	

	
show_gaussian(RandomManagerPid,Mu,Sigma) ->

	?test_debug([ io_lib:format( 
		"Requesting a gaussian random value with mean = ~w, deviation = ~w.", 
		[ Mu, Sigma ] ) ]),
		
	RandomManagerPid ! { getGaussianValue, [Mu, Sigma], self() },

	receive
	
		{wooper_result,{gaussian_value,Value}} ->
			?test_debug([ io_lib:format( 
				"Received a gaussian random value: ~w.", [ Value ] ) ])	
						
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
		
	FirstUniformTable = draw_uniform_values( 50, Values, MaxValue, 
		RandomManagerPid ),
			
	SecondUniformTable =
		draw_uniform_values( 500-50, FirstUniformTable,
			MaxValue, RandomManagerPid),

	ThirdUniformTable =
		draw_uniform_values( 5000-500, SecondUniformTable, MaxValue,
			RandomManagerPid),
		
	FourthUniformTable =
		draw_uniform_values( 50000-5000, ThirdUniformTable, MaxValue,
			RandomManagerPid ),

	Mean = compute_mean(FourthUniformTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"uniform distribution is ~w.", [ Mean ] ) ]).	
	
	
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
		
	FirstExponentialTable = draw_exponential_values( 50, Values, Lambda,
		RandomManagerPid ),
			
	SecondExponentialTable =
		draw_exponential_values( 500-50, FirstExponentialTable, Lambda,
			RandomManagerPid),

	ThirdExponentialTable =
		draw_exponential_values(5000-500, SecondExponentialTable, Lambda,
			RandomManagerPid),
		
	FourthExponentialTable =
		draw_exponential_values( 50000-5000, ThirdExponentialTable, 
			Lambda, RandomManagerPid ),

	Mean = compute_mean(FourthExponentialTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"exponential distribution is ~w.", [ Mean ] ) ]).

	
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
		draw_gaussian_values( 50, Values, Mu, Sigma, RandomManagerPid ),
		
	%io:format( "Gaussian table = ~w~n", [FirstGaussianTable] ),

	SecondGaussianTable =
		draw_gaussian_values( 500-50, FirstGaussianTable, Mu, Sigma,
			RandomManagerPid),

	ThirdGaussianTable =
		draw_gaussian_values(5000-500,SecondGaussianTable,Mu,Sigma,
			RandomManagerPid),
		
	FourthGaussianTable =
		draw_gaussian_values( 50000-5000, ThirdGaussianTable, 
			Mu, Sigma, RandomManagerPid ),
		
	Mean = compute_mean(FourthGaussianTable),
	 
	?test_info([ io_lib:format( "Mean of this full actual "
		"gaussian distribution is ~w.", [ Mean ] ) ]).


	
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

	Lambda = 0.1,
	test_exponential_random(RandomManagerPid,Lambda),

	Mu = 20, 
	Sigma = 10,
	test_gaussian_random(RandomManagerPid,Mu,Sigma),
		
	?test_info([ "Removing random manager." ]),
	class_RandomManager:remove(),

	?test_info([ "Removing time manager." ]),
	class_TimeManager:remove(),

	?test_stop.

