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


% This header centralizes notably all macros related to the sending traces.



% Conventions section.


% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter. 

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would be
% generally uselessly long and would cause issues in case of multiple
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

	-define( time,      ?system".Time" ).
	-define( random,    ?system".Random" ).
	-define( lifecycle, ?system".Lifecycle" ).


-define( execution, "Execution" ).

	-define( update, ?execution".Update" ).
	-define( state,  ?execution".State" ).

-define( DefaultMessageCategorization, ?execution".Uncategorized" ).


% Priority will be determined from the name of the chosen macro: fatal, error,
% warning, info, trace or debug.

% Message will be directly specified in the macro call.

 
-ifndef(TraceEmitterCategorization).
	-define( TraceEmitterCategorization, "NotCategorized" ).
-endif.



% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to tests.
% See also: test_constructs.hrl.
-define(DefaultTestMessageCategorization,"Test.Uncategorized").


% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to classical functions (as opposed to methods from class_TraceEmitter).
% See also: traces.hrl.
-define(DefaultStandaloneMessageCategorization,"Standalone.Uncategorized").


 
 
 
% Comment the next line if wanting to disable the trace output:
-define(TracingActivated,).

 
-ifdef(TracingActivated).


% The first version of macros uses an explicit state.
% The second version of macros uses an implicit state, named 'State', as, except
% in constructors, WOOPER conventions imply such a state exists and, provided
% informations stored in state have not changed (notably emitter name and
% categorization), the initial state declared in a method can be used instead of
% any newer one.
 
 

% Selecting a macro implies selecting a level of priority.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash.
% This explains why the trace informations have to be specified between brakets:
% using LOG([X,Y]) instead of LOG(X,Y).
% Anonymous functions could be used as well.
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each type of trace (fatal, error, warning, etc.), a few variations of the
% set of specified trace informations are supported.


% Previous (deprecate, not supported anymore) mode of operation.

% Previously, for a given type of trace K (ex: K might be 'fatal', or 'error',
% etc.), we could use three variations of the trace sending, knowing that the
% send_K macro requires a state to be specified and the K macro passes
% implicitly a state called 'State':
%
% Three trace informations:
%   ?send_K([ MyState,Message,MessageCategorization,Tick ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load,
% 132455 ]) 
%
%   ?K([Message,MessageCategorization,Tick])
%   Example: ?warning([ "This is my message", ?application_load, 132455 ]) 
%
% Two trace informations: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 'unknown'
% will be used.
%   ?send_K([ MyState,Message,MessageCategorization ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load ]) 
%
%   ?K([Message,MessageCategorization])
%   Example: ?warning([ "This is my message", ?application_load ]) 
%
% One trace information: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 'unknown'
% will be used; MessageCategorization is lacking too,
% DefaultMessageCategorization will be used instead.
%
%   ?send_K([MyState,Message])
%   Example: ?send_warning([ MyState,"This is my message" ]) 
%
%   ?K([Message])
%   Example: ?warning([ "This is my message" ]) %



% New mode of operation.

% The possibility of specifying in the macro call either 1, 2 or 3 arguments was
% seldom used. Instead we prefered defining more compact forms. Taking 'trace'
% as an example:
%
%  - '?trace([ "Hello" ])' becomes '?trace( "Hello" )': no more internal list
%  - '?trace([ "Hello", "My Category" ])' becomes 
% '?trace_cat( "Hello", "My Category" )' ('cat' stands for 'categorized')
%  - '?trace([ "Hello", "My Category", 125 ])' becomes 
% '?trace_full( "Hello", "My Category", 125 )' 
%
% Similarly, the use of io_lib:format involved too much typing, so we defined
% shorter forms instead. Taking 'trace' as an example:
%
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ) ])' becomes 
% '?trace_fmt( "Hello ~w.", [V] )' (most frequently used form; 'fmt' stands for
% 'format')
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ), "My Category" ])' becomes 
% '?trace_fmt_cat( "Hello ~w.", [V], "My Category" )'
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ), "My Category", 125 ])' 
% becomes '?trace_fmt_full( "Hello ~w.", [V], "My Category", 125 )'
%
% (knowing we cannot define macros with same name but different arity)



% Some delay are added when error traces are sent, so that they can be stored
% before the virtual machine is stopped, should it happen (ex: if an exception
% is thrown).
% Delays should better be replaced by synchronous operations. 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Fatal, without formatting.


% Plain, with 3 variations regarding state: explicit, implicit, none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal( State, Message ), 
		 class_TraceEmitter:send( fatal, State, Message ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal(Message),
		 class_TraceEmitter:send( fatal, State, Message ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal(Message),
		 class_TraceEmitter:send_standalone( fatal, Message ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized, with 3 variations regarding state: explicit, implicit, none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_cat( State, Message, MessageCategorization ), 
		 class_TraceEmitter:send( fatal, State, Message, 
								  MessageCategorization ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( fatal, State, Message, 
								  MessageCategorizatione ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( fatal, Message, 
											 MessageCategorization ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized with tick, with 3 variations regarding state: explicit, implicit,
% none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_full( State, Message, MessageCategorization, Tick ), 
		 class_TraceEmitter:send( fatal, State, Message, 
								  MessageCategorization, Tick ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State, Message, 
								  MessageCategorization, Tick ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_cat( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( fatal, Message, 
											 MessageCategorization, Tick ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).







% Subsection for Fatal, with formatting.


% Plain, with 3 variations regarding state: explicit, implicit, none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt( State, Message, FormatValues ), 
		 class_TraceEmitter:send( fatal, State, 
							  io_lib:format(Message,FormatValues) ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( fatal, State, 
							  io_lib:format(Message,FormatValues) ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format(Message,FormatValues) ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized, with 3 variations regarding state: explicit, implicit, none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt_cat( State, Message, FormatValues, 
							 MessageCategorization ), 
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues), 
							  MessageCategorization ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues), 
							  MessageCategorizatione ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fmt_fatal_cat( Message, FormatValues, 
							   MessageCategorization ),
		 class_TraceEmitter:send_standalone( fatal, 
							  io_lib:format(Message,FormatValues), 
							  MessageCategorization ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized with tick, with 3 variations regarding state: explicit, implicit,
% none.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt_full( State, Message, FormatValues, 
							  MessageCategorization, Tick ), 
		 class_TraceEmitter:send( fatal, State, 
							  io_lib:format(Message,FormatValues), 
							  MessageCategorization, Tick ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State, 
							  io_lib:format(Message,FormatValues), Tick ),
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_fmt_full( Message, FormatValues, MessageCategorization, 
								Tick ),
		 class_TraceEmitter:send_standalone( fatal, 
							  io_lib:format(Message,FormatValues), 
							  MessageCategorization, Tick ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).













% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error( State, Message ), 
		 class_TraceEmitter:send( error, State, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error(Message),
		 class_TraceEmitter:send( error, State, Message ), 
         % To ensure the asynchronous sending of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning( State, Message ), 
		 class_TraceEmitter:send( warning, State, Message ) ).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning(Message), 
		 class_TraceEmitter:send( warning, State, Message ) ).



% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info( State, Message ), 
		 class_TraceEmitter:send( info, State, Message ) ).


% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
-define( info(Message), 
		 class_TraceEmitter:send( info, State, Message ) ).



% Sends a trace of 'trace' type with specified parameters and an explicit state.
-define( send_trace( State, Message ), 
		 class_TraceEmitter:send( trace, State, Message ) ).


% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
-define( trace(Message), 
		 class_TraceEmitter:send( trace, State, Message ) ).



% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug( State, Message ), 
		 class_TraceEmitter:send( debug, State, Message ) ).


% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
-define( debug(Message), 
		 class_TraceEmitter:send( debug, State, Message ) ).




-define( notify_error(Message),
		 class_TraceEmitter:send_standalone( error, Message ),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).
	
	
-define( notify_warning(Message),
		 class_TraceEmitter:send_standalone( warning, Message )
).


-define( notify_info(Message),
		 class_TraceEmitter:send_standalone( info, Message )
).


-define( notify_trace(Message),
		 class_TraceEmitter:send_standalone( trace, Message )
).


-define( notify_debug(Message),
		 class_TraceEmitter:send_standalone( debug, Message )
).





-else.



% Traces are disabled here.
% This 'else' branch will be used iff TracingActivated is not defined below:


% Defined to avoid warnings about variables not being used.
% Hopefully will be inlined, and then optimized out as a whole by the compiler.
trace_disabled( _State, _Message ) ->
	trace_disabled.

trace_disabled( _Message ) ->
	trace_disabled.



% Most important trace categories cannot be disabled:

-define( send_fatal( State, Message ),
		 io:format( "Fatal trace message (although traces are disabled): ~s.~n",
					[Message] ),
         % To ensure the asynchronous output of the trace has a chance to
         % complete, possibly before the interpreter is crashed:
		 timer:sleep(100),
		 trace_disabled( State )
		
).

-define( fatal(Message), 
		 ?send_fatal( undefined, Message ) ).



-define( send_error( State, Message ),
		 io:format( "Error trace message (although traces are disabled): ~s.~n",
					[Message] ),
	     % To ensure the asynchronous output of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100),
		 trace_disabled( State )
).


-define( error(Message), 
		 ?send_error( undefined, Message ) ).



-define( send_warning( State, Message ), 
		 trace_disabled( State, Message ) ).


-define( warning(Message), 
		 trace_disabled(Message) ).



-define( send_info( State, Message ), 
		 trace_disabled(State,Message) ).


-define( info(Message), 
		 trace_disabled(Message) ).



-define( send_trace( State, Message ), 
		 trace_disabled( State, Message ) ).


-define( trace(Message), 
		 trace_disabled(Message) ).



-define( send_debug( State, Message ), 
		 trace_disabled( State, Message ) ).


-define( debug(Message), 
		 trace_disabled(Message) ).



% To send traces neither from a TraceEmitter instance nor from a test
% (ex: in a static method):

-define( notify_fatal(Message),
		 io:format( "Fatal trace message (although traces are disabled):~s.~n",
					[Message]),
	     % To ensure the asynchronous output of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


-define( notify_error(Message),
		 io:format( "Error trace message (although traces are disabled):~s.~n",
					[Message]),
	     % To ensure the asynchronous sending of the trace has a chance to
	     % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).
	
	
-define( notify_warning(Message), 
		 trace_disabled(Message) ).


-define( notify_info(Message), 
		 trace_disabled(Message) ).


-define( notify_trace(Message), 
		 trace_disabled(Message) ).


-define( notify_debug(Message), 
		 trace_disabled(Message) ).


-endif.

% End of the TracingActivated branch.


