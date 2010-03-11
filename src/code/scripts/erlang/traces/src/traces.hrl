% Copyright (C) 2003-2010 Olivier Boudeville
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
% reason, like needing to generate a report; TraceTextOutputType can be:
%     - 'text_only', if wanting to have traces be directly written to disk
% as pure yet human-readable text
%     - 'pdf', if wanting to read finally the traces in a generated PDF file
%
% Note: if you change (ex: comment/uncomment) the trace type, then you must
% recompile your modules to take it into account. 
-ifndef(TraceType).
	-define(TraceType,log_mx_traces).
	%-define(TraceType,{text_traces,text_only}).
	%-define(TraceType,{text_traces,pdf}).
-endif.



% Defines the trace title (ex: for PDF output), if not already specified:
-ifndef(TraceTitle).
	-define(TraceTitle,"Ceylan").
-endif.



% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").	



% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code). 
% Note: using 'emit' instead of 'send' to prevent name clashes.

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

