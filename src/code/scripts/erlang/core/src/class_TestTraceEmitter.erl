% Test of TraceEmitter class.
% See class_TraceEmitter.hrl and class_TraceEmitter.erl.
-module(class_TestTraceEmitter).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,TraceEmitterName).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).

% Method declarations.
-define(wooper_method_export,sendTraces/1).



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
	
	
	
% Overriden destructor.
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

	% Then call the direct mother class counterparts:
	DeletedTraceEmitterState = class_TraceEmitter:delete(State),
	
	io:format( "~s Test trace emitter ~s deleted.~n", 
		[ ?LogPrefix, ?getAttr(name) ] ),
	
	% Allow chaining:
	DeletedTraceEmitterState.

	
	
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
	
	?fatal(   ["Ouh-ouh-ouuuuuh fatal",   ?sim_start    ] ),
	?error(   ["Ouh-ouh-ouuuuuh error",   ?sim_save     ] ),
	?warning( ["Ouh-ouh-ouuuuuh warning", ?time         ] ),
	?info(    ["Ouh-ouh-ouuuuuh info",    ?simulation   ] ),
	?trace(   ["Ouh-ouh-ouuuuuh trace",   ?sim_start    ] ),
	?debug(   ["Ouh-ouh-ouuuuuh debug",   ?sim_start    ] ),
	
	
	?fatal(   ["Oh year fatal",   ?sim_start, 5     ] ),
	?error(   ["Oh year error",   ?sim_save, 6      ] ),
	?warning( ["Oh year warning", ?time, 7          ] ),
	?info(    ["Oh year info",    ?simulation, 8    ] ),
	?trace(   ["Oh year trace",   ?sim_start, 9     ] ),
	?debug(   ["Oh year debug",   ?sim_start, 10] ),
	
	?wooper_return_state_result(State,ok).

