% This header centralizes all simulation-wide settings.




% Separating mute/by speak allows to use different notifications
% (ex: shorter if spoken, for example ticks are far too long to be spoken).
% As notify_mute needs an implicit 'State' variable, it cannot be a function,
% it is therefore a macro, and notify_by_speak too for homogeneity.

-define( notify_mute(Message),
	?info([ Message ])
).



% Tells whether the simulation system should be talkative.
% (uncomment to mute)
%-define(talkative,).

-ifdef(talkative).		
		
-define( notify_by_speak(Message),
	utils:speak(Message)
).
		
-else.

-define(notify_by_speak(Message),
	speak_muted
).
	
-endif.


-define( notify(Message),
	?notify_mute(Message),
	?notify_by_speak(Message)
).

