% Management of a simulation.
% The simulation manager is expected to be a singleton.
% It manages the whole simulation, including the scenario and time managers.
% Its process is registered under the name ?simulation_manager_name.
-module(class_SimulationManager).



% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,SimulationFrequency,Interactive,
	ScenarioType,SimulationListenerPid).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/4,new_link/4,
	synchronous_new/4,synchronous_new_link/4,construct/5,delete/1).

% Method declarations.
-define(wooper_method_export,start/1,stop/1,simulationEnded/2).

% Static method declarations (to be directly called from module):
-export([remove/0,start/0,stop/0,getManager/0]).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"SimulationManagement").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For talkative settings:
-include("simulation_settings.hrl").

% For simulation_manager_name:
-include("class_SimulationManager.hrl").


% Use global:registered_names() to check manager presence.


% Implementation notes:
% The simulation manager is at the root of the simulation process hierarchy, 
% and will manage other managers (ex: the TimeManager).


% Constructs a new simulation manager.
%  - SimulationFrequency is the desired frequency of simulation steps, for the
% TimeManager that will be created 
%  - Interactive can be true (then the simulation will try to run on par with 
% the user time), or false (then the simulation will run as fast as possible)
%  - ScenarioType describes which scenario will be simulated
%  - SimulationListenerPid the PID of a process that will be notified of the
% main simulation events by this manager
% Creates the scenario manager, and send it the ScenarioType.
% Creates the time manager.
% Records a simulation listener, so that the caller (ex: the root test process)
% can be notified of a simulation shutdown. 
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, "SimulationManager" ),

	CategorizedState = ?setAttributes( TraceState, [
		{trace_categorization,?TraceEmitterCategorization},
		{started,false}, {stop_requested,false}, {interactive,Interactive},
		{simulation_listener,SimulationListenerPid} ]),

	?send_trace([ CategorizedState, "Creating simulation manager" ]),
	
	case global:register_name( ?simulation_manager_name, self() ) of 
	
		yes ->
			?send_debug([ CategorizedState, 
				"Simulation manager registered globally" ]);
				
		no ->
			?send_error([ CategorizedState, 
				"Simulation manager could not be registered." ]),	
			exit( simulation_manager_could_not_register )
			
	end,
	
	% Registers as well locally:
	true = register( ?simulation_manager_name, self() ),
	
	?send_debug([ CategorizedState, "Launching managers." ]),
	
	ManagerState = ?setAttributes( CategorizedState, [
		{ time_manager_pid,
			class_TimeManager:new_link( SimulationFrequency, Interactive ) },
		{ scenario_manager_pid,
			class_ScenarioManager:new_link( ScenarioType, self() ) },
		{ random_manager_pid, class_RandomManager:create() } ]),
			
	?send_debug([ CategorizedState, "Waiting for scenario to be ready." ]),

	receive
	
		scenario_ready ->
			SimulationListenerPid ! simulation_ready_to_run	
		
	end,
	
	?send_debug([ ManagerState, "Simulation manager created." ]),
	ManagerState.

	
% Overriden destructor.
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting  manager." ]),
	stop(State),
	unregister( ?simulation_manager_name ),
	global:unregister_name( ?simulation_manager_name ),

	?trace([ "Simulation manager deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	
	

% Methods section.



% Management section of the simulation manager.


% Starts the simulation manager.
% Returns as soon as the start notification has been sent to the time manager.	
% (oneway)
start(State) ->

	?notify( "Starting simulation." ),
	
	% This manager will not terminate when an exit signal is received, instead
	% the signal will be transformed into an 'EXIT' message: 
	% (a monitor could be used instead)
	erlang:process_flag(trap_exit, true),
	
	StartState = ?setAttributes( State, [ 
		{stop_requested,false}, {started,true} ] ),
		
	?getAttr(time_manager_pid) ! start,
	?wooper_return_state_only(StartState).					
				
			 
			 
% Stops the simulation manager.	
% (oneway)
stop(State) ->

	?notify( "Stopping simulation." ),

	?getAttr(time_manager_pid) ! stop,

	StoppedState = ?setAttributes( State, [
		{stop_requested,true}, {started,false} ]),
	
	?wooper_return_state_only(StoppedState).



% Oneway generally called by the scenario manager to notify it the simulation
% is over. 
% (oneway)
simulationEnded(State,EndTick) ->

	?notify_mute( io_lib:format( "Simulation ended at tick ~B.", 
		[ EndTick ] ) ),
		
	?notify_by_speak( "Simulation ended." ),

	self() ! stop(State),
	
	% Non-WOOPER listener:
	?getAttr(simulation_listener) ! {simulation_ended,EndTick},
	
	?wooper_return_state_only(State).
	
	
	
% 'Static' methods (module functions):

	
% Starts the already-created simulation manager.
% (static)
start() ->

	% Allows to create and start directly afterwards:
	case utils:wait_for_global_registration_of( ?simulation_manager_name ) of
	
		{registration_waiting_timeout,?simulation_manager_name} ->
			simulation_manager_not_found;
			
		SimulationManagerPid when is_pid(SimulationManagerPid) ->
			SimulationManagerPid ! start
			
	end.
	

% Stops the simulation manager.
% (static)	
stop() ->

	case global:whereis_name( ?simulation_manager_name ) of
	
		undefined ->
			simulation_manager_not_found;
			
		SimulationManagerPid when is_pid(SimulationManagerPid) ->
			SimulationManagerPid ! stop
			
	end.


% Deletes the simulation manager.	
% (static)	
remove() ->

	case global:whereis_name( ?simulation_manager_name ) of
	
		undefined ->
			simulation_manager_not_found;
			
		SimulationManagerPid when is_pid(SimulationManagerPid) ->
			SimulationManagerPid ! delete
			
	end.


% Returns the Pid of the current simulation manager if it exists, otherwise 
% simulation_manager_not_found. 
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
% (static)
getManager() ->

	% Waits gracefully for the simulation manager to exist:
	case utils:wait_for_global_registration_of( ?simulation_manager_name ) of
	
		{registration_waiting_timeout,?simulation_manager_name} ->
			simulation_manager_not_found;
			
		SimulationManagerPid when is_pid(SimulationManagerPid) ->
			SimulationManagerPid
			
	end.
	


% Section for helper functions (not methods).
			
