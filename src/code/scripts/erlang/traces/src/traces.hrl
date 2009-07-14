% 
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

