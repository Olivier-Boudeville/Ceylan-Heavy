% Management of a scenario.
% The scenario manager is expected to be a singleton.
% It is a time listeners, and behaves like a simulation element, triggered
% by the same clock.
% Its process is registered under the name ?scenario_manager_name.
-module(class_ScenarioManager).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,ScenarioType,SimulationManagerPid).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).


% Method declarations.
-define(wooper_method_export,act/1,start/1,stop/1,evaluateScenario/1).


% Static method declarations (to be directly called from module):
%-export([]).

% Helper functions:
%-export([]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Test").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For scenario_manager_name:
-include("class_ScenarioManager.hrl").


% Use global:registered_names() to check manager presence.



% Constructs a new scenario manager.
%  - ScenarioType is an atom allowing to choose a target scenario
%  - SimulationManagerPid is needed so that this scenario manager can report 
% progress to the simulation manager.
% The time manager is expected to be already running.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, "Scenario Manager" ),

	?send_info([ ActorState, "Creating a new scenario manager." ]),

	% A dedicated probe could be instantiated here.
	StartState = ?setAttributes( ActorState, [ {probe,undefined},
		{private_random_manager,class_RandomManager:new_link(0,1,true)},
		{scenario_type,ScenarioType},
		{simulation_manager_pid,SimulationManagerPid},
		{trace_categorization,?TraceEmitterCategorization} ] ),
			
	case global:register_name( ?scenario_manager_name, self() ) of 
	
		yes ->
			?send_debug([ StartState,
				"Scenario manager registered globally." ]);
		
		no ->
			?send_error([ StartState,
				"Scenario manager could not be registered." ]),
			exit( scenario_manager_could_not_register )
			
	end,
	
	% Registers as well locally:
	true = register( ?scenario_manager_name, self() ),
	
	ScenarioState = case ScenarioType of
	
		fixed_length_scenario ->
			Duration = ?default_scenario_duration,
			?send_debug([ StartState, io_lib:format( 
				"Scenario Manager will stop after ~B ticks.", [ Duration ] ) ]),
			?setAttribute( StartState, termination_tick_offset, Duration );
			
		_ ->
			% By default a scenario has unlimited timespan:
			?send_debug([ StartState, 
				"Scenario Manager will have an unlimited timespan." ]),
			?setAttribute( StartState, termination_tick_offset, infinite )
		
	end,

	% Start may be overloaded:
	{wooper_result,StartedState} = executeOneway(ScenarioState,start),
		
	?send_debug([ StartedState, "Scenario Manager created." ]),
	StartedState.


	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting scenario manager." ]),
	unregister( ?scenario_manager_name ),
	global:unregister_name( ?scenario_manager_name ),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Scenario manager deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).

	
	

% Methods section.


% Management section of the scenario.

% Starts this scenario manager.
% (oneway)
start(State) ->
	?getAttr(simulation_manager_pid) ! scenario_ready,
	?wooper_return_state_only( State ).
	

% Stops this scenario manager.
% (oneway)
stop(State) ->
	case ?getAttr(probe) of
			
		undefined -> 
			ok ;
				
		ProbePid ->
			ProbePid ! generateReport,
			receive
	
				{wooper_result,probe_report_generated} ->
					?trace([ "Scenario report correctly generated." ])
					
			end
				
	end,
			
	?getAttr(simulation_manager_pid) ! { simulationEnded,
		class_TraceEmitter:get_current_tick(State) },

	?wooper_return_state_only( State ).
	



% The core of the scenario behaviour.
% The scenario manager terminates directly, and stops on time manager
% shutdown (that it may trigger).
act(State) ->

	?debug([ "Scenario manager acting." ]),
	
	TerminationOffset = ?getAttr(termination_tick_offset),
	
	% Terminates if the termination offset is reached:		
	UpdatedState = case ?getAttr(current_time_offset) of 
	
		TerminationOffset ->

			?debug([ "Preparing simulation termination now "
				"(requested duration reached)." ]),
			
			% Stop may be overloaded:
			{wooper_result,StoppedState} = executeOneway(State,stop),
			
			% Terminates, as any actor:		
			?setAttribute(StoppedState,termination_mode,terminated);
			
		_ ->
			% This (probably overriden) oneway returns a new state:
			{wooper_result,ScenarioEvaluatedState} =
				executeOneway(State,evaluateScenario),
			ScenarioEvaluatedState
			
	end,
	?wooper_return_state_only( class_Actor:manage_end_of_tick( UpdatedState ) ).
			
	
	
% 'Static' methods (module functions):



% Section for helper functions (not methods).
		


% The actual implementation of the scenario.	
% Creates an information system, then a set of low-level meshes.
% Returns an updated state.
% (oneway)
evaluateScenario(State) ->

	?debug([ "Scenario being evaluated (non-overriden)." ]),
		
	% Example:
	
	%CurrentTick = class_TraceEmitter:get_current_tick(State),	
	%% Sends stats to probe every 5 ticks:
	%case CurrentTick rem 5 of 
	%
	%	0 -> 
	%		?getAttr(probe) ! {setData, 
	%			[?getAttr(current_time_offset), {
	%				...
	%				} ]};
    %
	%	_ ->
	%		ok
	%		
	%end,	
	?wooper_return_state_only(State).	

