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


% This header centralizes notably all macros related to the sending traces.


% Comment out to deactive tracing:
-define(TracingActivated,true).



-ifdef(TracingActivated).


-ifndef(TraceEmitterCategorization).
	-define( TraceEmitterCategorization, "NotCategorized" ).
-endif.



% Conventions section.


% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter. 

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would
% be generally uselessly long and would cause issues in case of multiple
% inheritance.

% Execution timestamp (tick) will be either specified in the send macro (a long
% integer or 'none'), or set to following default execution timestamp:
-define( DefaultExecutionTimestamp, "unknown" ).


% User timestamp will be the current date, as determined by the trace emitter.

% Location will be the current Erlang node, as determined by the trace emitter.

% MessageCategorization will be either specified in the send macro, or 
% set the default categorization.

% Following built-in message categorizations are available:

-define( system, "System" ).

	-define( management, ?system".Management" ).

		-define( application_start,    ?management".ExecutionStart" ).
		-define( application_resume,   ?management".ExecutionResume" ).
		-define( application_continue, ?management".ExecutionContinue" ).
		-define( application_stop,     ?management".ExecutionStop" ).
		-define( application_save,     ?management".ExecutionSave" ).
		-define( application_load,     ?management".ExecutionLoad" ).

	-define( time,       ?system".Time" ).
	-define( random,     ?system".Random" ).
	-define( lifecycle,  ?system".Lifecycle" ).


-define( execution, "Execution" ).

	-define( update,        ?execution".Update" ).
	-define( state,         ?execution".State" ).

-define( DefaultMessageCategorization, ?execution".Uncategorized" ).


% Priority will be determined from the name of the chosen macro:
% fatal, error, warning, info, trace or debug.

% Message will be directly specified in the macro call.

 
% The first version of macros uses an explicit state.
% The second version of macros uses an implicit state, named 'State', as,
% except in constructors, WOOPER conventions imply such a state exists and,
% provided informations stored in state have not changed (notably emitter name
% and categorization), the initial state declared in a method can be used
% instead of any newer one.
 
 

% Selecting a macro implies selecting a level of priority.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash. 
% This explains why the trace informations have to be specified between
% brakets: using LOG([X,Y]) instead of LOG(X,Y).
% Anonymous functions could be used as well.
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each type of trace (fatal, error, warning, etc.), a few variations
% of the set of specified trace informations are supported. 


% For a given type of trace K (ex: K might be 'fatal', or 'error', etc.), we 
% can call, knowing that the send_K macro requires a state to be specified 
% and the K macro passes implicitly a state called 'State':
%
% Three trace informations:
%   ?send_K([ MyState,Message,MessageCategorization,Tick ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load, 132455 ]) 
%
%   ?K([Message,MessageCategorization,Tick])
%   Example: ?warning([ "This is my message", ?application_load, 132455 ]) 
%
% Two trace informations: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 
% 'unknown' will be used.
%   ?send_K([ MyState,Message,MessageCategorization ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load ]) 
%
%   ?K([Message,MessageCategorization])
%   Example: ?warning([ "This is my message", ?application_load ]) 
%
% One trace information: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 
% 'unknown' will be used; MessageCategorization is lacking too, 
% DefaultMessageCategorization will be used instead.
%
%   ?send_K([MyState,Message])
%   Example: ?send_warning([ MyState,"This is my message" ]) 
%
%   ?K([Message])
%   Example: ?warning([ "This is my message" ]) 
%



% Some delay are added when error traces are sent, so that they can be stored
% before the virtual machine is stopped, should it happen (ex: if an exception
% is thrown).
% Delays should better be replaced by synchronous operations. 


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal(Args), 
	class_TraceEmitter:send(fatal,Args),
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(100)
).



% Sends a trace of 'fatal' type with specified parameters and implicit 'State'.
-define( fatal(Args),
	class_TraceEmitter:send(fatal,[State|Args]),
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(100)
).




% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error(Args), 
	class_TraceEmitter:send(error,Args),
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(100)
).

	

% Sends a trace of 'error' type with specified parameters and implicit 'State'.
-define( error(Args),
	class_TraceEmitter:send(error,[State|Args]), 
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(100)
).


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning(Args), class_TraceEmitter:send(warning,Args) ).

% Sends a trace of 'warning' type with specified parameters and implicit
% 'State'.
-define( warning(Args), class_TraceEmitter:send(warning,[State|Args]) ).


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info(Args), class_TraceEmitter:send(info,Args) ).

% Sends a trace of 'info' type with specified parameters and implicit 'State'.
-define( info(Args), class_TraceEmitter:send(info,[State|Args]) ).


% Sends a trace of 'trace' type with specified parameters and an explicit state.
-define( send_trace(Args), class_TraceEmitter:send(trace,Args) ).

% Sends a trace of 'trace' type with specified parameters and implicit 'State'.
-define( trace(Args), class_TraceEmitter:send(trace,[State|Args]) ).


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug(Args), class_TraceEmitter:send(debug,Args) ).

% Sends a trace of 'debug' type with specified parameters and implicit 'State'.
-define( debug(Args), class_TraceEmitter:send(debug,[State|Args]) ).



-else.



-define( send_fatal(Args), trace_disabled ).
-define( fatal(Args), trace_disabled ).

-define( send_error(Args), trace_disabled ).
-define( error(Args), trace_disabled ).

-define( send_warning(Args), trace_disabled ).
-define( warning(Args), trace_disabled ).

-define( send_info(Args), trace_disabled ).
-define( info(Args), trace_disabled ).

-define( send_trace(Args), trace_disabled ).
-define( trace(Args), trace_disabled ).

-define( send_debug(Args), trace_disabled ).
-define( debug(Args), trace_disabled ).


-endif.



% To send traces neither from a TraceEmitter instance nor from a test
% (ex: in a static method):

-define( notify_fatal(Message),
	class_TraceEmitter:send_standalone(fatal,[Message]),
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(100)
).


-define( notify_error(Message),
	class_TraceEmitter:send_standalone(error,[Message]),
	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	timer:sleep(10)
).
	
	
-define( notify_warning(Message),
	class_TraceEmitter:send_standalone(warning,[Message])
).


-define( notify_info(Message),
	class_TraceEmitter:send_standalone(info,[Message])
).


-define( notify_trace(Message),
	class_TraceEmitter:send_standalone(trace,[Message])
).


-define( notify_debug(Message),
	class_TraceEmitter:send_standalone(debug,[Message])
).



% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to tests.
% See also: test_constructs.hrl.
-define(DefaultTestMessageCategorization,"Test.Uncategorized").


% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to classical functions (as opposed to methods from class_TraceEmitter).
% See also: traces.hrl.
-define(DefaultStandaloneMessageCategorization,"Standalone.Uncategorized").

