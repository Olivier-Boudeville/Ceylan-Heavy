% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Extension to be used for trace file names:
-define( TraceExtension, ".traces" ).


% Per-test trace file (must be defined before the TraceSupervisor include):
-define( TraceFilename, ( atom_to_list(?MODULE) ++ ?TraceExtension ) ).



% Defines the type of requested execution traces. 
% The trace type can be either:
%  - log_mx_traces, for LogMX-compliant traces (the default): then the trace
% aggregator will use a proper encoding so that the Ceylan Java trace parser,
% plugged into LogMX, allows this tool to be used with Ceylan
%  - {text_traces,TraceTextOutputType} for more basic text-based traces: then
% the trace aggregator will do its best to format the traces as a human-readable
% trace text file; this is mostly useful when LogMX cannot be used for any
% reason; TraceTextOutputType can be either 'text_only' (if just wanting to
% display a text file), or 'pdf' (if wanting to read the traces from a PDF
% file).
-ifndef(TraceType).
	-define(TraceType,log_mx_traces).
	%-define(TraceType,{text_traces,text_only}).
	%-define(TraceType,{text_traces,pdf}).
-endif.


% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").	




-define(traces_start, 
	% Create first, synchronously (to avoid race conditions), a trace
	% aggregator (false is to specify a non-private i.e. global aggregator).
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found).
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		?TraceFilename, ?TraceType, false )
).



-define(traces_stop, 
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive
	
  		{deleted,TraceAggregatorPid} ->
			ok
			
  	end
	%check_pending_wooper_results()
).





% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code). 
% Note: using 'emit' instead of 'send' to prevent name clashes.


% Section for the sending of an uncategorized message.
% Usage: '?emit_debug([ "Starting!" ])'


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



% Section for the sending of a categorized message.
% Usage: '?notify_debug([ "Starting!", "My Emitter Name", 
% "My Emitter Category" ])'


-define( notify_fatal( Message, TraceEmitterName, TraceEmitterCategorization ),
	class_TraceEmitter:send_standalone( fatal,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).


-define( notify_error( Message, TraceEmitterName, TraceEmitterCategorization ),
	class_TraceEmitter:send_standalone( error,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).
	
	
-define( notify_warning( Message, TraceEmitterName, TraceEmitterCategorization
		),
	class_TraceEmitter:send_standalone( warning,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).


-define( notify_info( Message, TraceEmitterName, TraceEmitterCategorization ),
	class_TraceEmitter:send_standalone( info,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).


-define( notify_trace( Message, TraceEmitterName, TraceEmitterCategorization ),
	class_TraceEmitter:send_standalone( trace,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).


-define( notify_debug( Message, TraceEmitterName, TraceEmitterCategorization ),
	class_TraceEmitter:send_standalone( debug,
		[ Message, TraceEmitterName, TraceEmitterCategorization ] ) ).

