
% The name under which the time manager process is to be registered:
% (use ?time_manager_name to have it)
-define(time_manager_name,ceylan_time_manager).


% Simulation frequency, in Hertz (Hz). Ranges in [1..1000 Hz].
%
% It corresponds to the number of ticks in one virtual second.
% (ex: 50 Hz corresponds to a period of 20 ms in virtual time)
%
% @note This frequency will be converted internally to nearest integer period,
% expressed in milliseconds. Frequency should be at most 1 kHz.
%
% @example to have one tick per virtual second (smallest allowed frequency), 
% use simply 1 Hz.
%
-define(default_simulation_frequency,2).


% We can be in interactive mode without having to make the real and 
% virtual units correspond one-to-one. Virtual time can be scaled up or down
% compared to real time, ex: 1 second in user time representing 1 simulated day,
% (thus for a value of 3600*24) while still being interactive.  
-define(scale_factor_for_interactive_time,1).


