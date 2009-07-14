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


% Test of TraceEmitter class.
% See class_TraceEmitter.hrl and class_TraceEmitter.erl.
-module(class_TestTraceEmitter).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters, TraceEmitterName ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1, 
	synchronous_new/1, synchronous_new_link/1,
	synchronous_timed_new/1, synchronous_timed_new_link/1,
	remote_new/2, remote_new_link/2, remote_synchronous_new/2,
	remote_synchronous_new_link/2, remote_synchronous_timed_new/2,
	remote_synchronous_timed_new_link/2, construct/2, delete/1 ).



% Method declarations.
-define(wooper_method_export, sendTraces/1 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Only to test the trace system:
-define(LogPrefix,"[Test TraceEmitter]").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"TraceEmitter.Test").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

	
% Constructs a new test trace emitter.
construct(State,?wooper_construct_parameters) ->
	io:format( "~s Creating a new test trace emitter, whose name is ~s, "
		"whose PID is ~w.~n", [ ?LogPrefix, TraceEmitterName, self() ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, TraceEmitterName ),
		
	% Class-specific:
	TestTraceState = ?setAttribute( TraceState, trace_categorization,
		?TraceEmitterCategorization ),
		 
	% From now on, traces can be sent (but from the constructor send_* traces
	% only should be sent, to be able to refer to a trace-enabled state);
	?send_fatal(   [TestTraceState,"Hello fatal world! "]   ),
	?send_error(   [TestTraceState,"Hello error world! "]   ),
	?send_warning( [TestTraceState,"Hello warning world! "] ),
	?send_info(    [TestTraceState,"Hello info world! "]    ),
	?send_trace(   [TestTraceState,"Hello trace world! "]   ),
	?send_debug(   [TestTraceState,"Hello debug world! "]   ),
	TestTraceState.
	
	
	
% Overridden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	io:format( "~s Deleting test trace emitter ~s.~n", 
		[ ?LogPrefix, ?getAttr(name) ] ),
	% erlang:unlink() not used, as done manager-side. 

	% Last moment to send traces:
	?fatal(   ["Goodbye fatal world! "  ] ),
	?error(   ["Goodbye error world! "  ] ),
	?warning( ["Goodbye warning world! "] ),
	?info(    ["Goodbye info world! "   ] ),
	?trace(   ["Goodbye trace world! "  ] ),
	?debug(   ["Goodbye debug world! "  ] ),
	
	io:format( "~s Test trace emitter ~s deleted.~n", 
		[ ?LogPrefix, ?getAttr(name) ] ),
	% Allow chaining:
	State.

	
	
% Methods section.

% (request)
sendTraces(State) ->

	io:format( "~s Sending some traces.~n",	[ ?LogPrefix ] ),
	
	?fatal(   ["Still livin' in a fatal world! "  ] ),
	?error(   ["Still livin' in a error world! "  ] ),
	?warning( ["Still livin' in a warning world! "] ),
	?info(    ["Still livin' in a info world! "   ] ),
	?trace(   ["Still livin' in a trace world! "  ] ),
	?debug(   ["Still livin' in a debug world! "  ] ),
	
	?fatal(   ["Ouh-ouh-ouuuuuh fatal",   ?application_start    ] ),
	?error(   ["Ouh-ouh-ouuuuuh error",   ?application_save     ] ),
	?warning( ["Ouh-ouh-ouuuuuh warning", ?time               ] ),
	?info(    ["Ouh-ouh-ouuuuuh info",    ?execution          ] ),
	?trace(   ["Ouh-ouh-ouuuuuh trace",   ?application_start    ] ),
	?debug(   ["Ouh-ouh-ouuuuuh debug",   ?application_start    ] ),
	
	
	?fatal(   ["Oh yeah fatal",   ?application_start,  5 ] ),
	?error(   ["Oh yeah error",   ?application_save,   6 ] ),
	?warning( ["Oh yeah warning", ?time,             7 ] ),
	?info(    ["Oh yeah info",    ?execution,        8 ] ),
	?trace(   ["Oh yeah trace",   ?application_start,  9 ] ),
	?debug(   ["Oh yeah debug",   ?application_start, 10 ] ),
	
	?wooper_return_state_result(State,ok).

