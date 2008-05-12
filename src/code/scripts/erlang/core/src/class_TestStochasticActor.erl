% Test of Stochastic Actor class, regarding random management.
% Note: to be used with randomManagerAndStochasticActorPair_test.erl.
-module(class_TestStochasticActor).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_StochasticActor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,ActorName,ListOfRandomLists,
	TerminationTickOffset).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/3,new_link/3,
	synchronous_new/3,synchronous_new_link/3,construct/4,delete/1).

% Method declarations.
-define(wooper_method_export,act/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"StochasticActor.Test").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

	
	
% Constructs a new test actor.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	StochasticState = class_StochasticActor:construct( State, ActorName,
		ListOfRandomLists ),

	?send_info([ StochasticState, "Creating a new test stochastic actor." ]),
	
	% Prepare different lists of random values:
	?setAttributes( StochasticState, [
		{trace_categorization,?TraceEmitterCategorization},
		{termination_tick_offset,TerminationTickOffset} ] ).
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting test stochastic actor." ]),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Stochastic actor deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_StochasticActor:delete(State).
	
	
	

% Methods section.


% Management section of the actor.
	
				

% The core of the test stochastic actor behaviour.
% (oneway)
act(State) ->

	?info([ "Test Actor acting" ]),
	
	TerminationOffset = ?getAttr(termination_tick_offset),
	
	% Terminates iff the termination offset is reached:		
	UpdatedState = case ?getAttr(current_time_offset) of 
	
		TerminationOffset ->
		
			?info([ "Test Actor preparing termination." ]),
			?setAttribute(State,termination_mode,terminated);
				
			
		_ ->
			case ?getAttr(initialization_status) of 
			
				completed ->
				
					?info([ "Test Actor eating random lists." ]),
						
					% At each tick two random values are popped from each 
					% uniform random list, one value from each other list.
					% List names are hardcoded in this tests, see
					% randomManagerAndStochasticActorPair_test.erl.
					% Each list has a different minimum size from the others.
					{FirstUpdatedState,FirstRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_first_uniform, State ),
						
					{SecondUpdatedState,SecondRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_first_uniform, FirstUpdatedState ),
							
					{ThirdUpdatedState,ThirdRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_second_uniform, SecondUpdatedState ),
							
					{FourthUpdatedState,FourthRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_second_uniform, ThirdUpdatedState ),
						
					{FifthUpdatedState,FifthRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_gaussian, FourthUpdatedState ),
						
					{SixthUpdatedState,SixthRandomValue} =
						class_StochasticActor:get_random_value_from(
							my_exponential, FifthUpdatedState ),
						
					?info([ io_lib:format( 
						"Test Actor drew ~w from first uniform list, "
						"~w from second, ~w from gaussian one and "
						"~w from exponential one.", 
						[ {FirstRandomValue,SecondRandomValue},	
							{ThirdRandomValue,FourthRandomValue},
							 FifthRandomValue, SixthRandomValue ] ) ]),
							
					SixthUpdatedState;
					
					
				waiting ->
					?info([ "Test Actor eating random lists." ]),
					State;
						
						
				blank ->
					?info([ "Test Actor requesting random lists." ]),
							
					% An actor message will be sent iff there is at least one
					% random list. 
					InitState = 
						class_StochasticActor:initialize_random_buffers(State),
						 
					?setAttribute( InitState, initialization_status, waiting )
											
			end
			
			
	end,
	?wooper_return_state_only( class_Actor:manage_end_of_tick( UpdatedState ) ).
			
	
	
% Section for helper functions (not methods).

