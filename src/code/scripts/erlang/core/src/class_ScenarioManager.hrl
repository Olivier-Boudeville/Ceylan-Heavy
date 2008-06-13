
-ifndef(scenario_manager_name).

% The name under which the scenario manager process is to be registered:
% (use ?scenario_manager_name to have it)
-define(scenario_manager_name,ceylan_scenario_manager).

-endif.


% Determines how many ticks a fixed-length scenario should last by default:
-define(default_scenario_duration,100).

