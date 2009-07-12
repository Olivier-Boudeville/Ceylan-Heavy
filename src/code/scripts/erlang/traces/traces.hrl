% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code) 
% Note: using 'emit' instead of 'send' to prevent name clashes.

% Usage: '?emit_debug([ "Starting !" ])'


-define( emit_fatal(Message),
	class_TraceEmitter:send_standalone(fatal,[Message])
).


-define( emit_error(Message),
	class_TraceEmitter:send_standalone(error,[Message])
).
	
	
-define( emit_warning(Message),
	class_TraceEmitter:send_standalone(warning,[Message])
).


-define( emit_info(Message),
	class_TraceEmitter:send_standalone(info,[Message])
).


-define( emit_trace(Message),
	class_TraceEmitter:send_standalone(trace,[Message])
).


-define( emit_debug(Message),
	class_TraceEmitter:send_standalone(debug,[Message])
).

