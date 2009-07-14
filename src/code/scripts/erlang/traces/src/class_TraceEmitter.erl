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


% Base class for all emitters of traces.
% A trace emitter has a notion of time (execution tick) as it needs to
% timestamp its traces.
% See class_TestTraceEmitter.erl and class_TraceEmitter_test.erl
-module(class_TraceEmitter).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct'). 
% These are class-specific data needing to be set in the constructor:
% (TraceEmitterCategorization will be set in the trace_categorization attribute
% of each child class when coming down the inheritance hierarchy, so that the
% latest child class sets its targeted trace_categorization value)
-define( wooper_construct_parameters, TraceEmitterName ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-export([ new/1, new_link/1, synchronous_new/1, synchronous_new_link/1,
	synchronous_timed_new/1, synchronous_timed_new_link/1,
	remote_new/2, remote_new_link/2, remote_synchronous_new/2,
	remote_synchronous_new_link/2, remote_synchronous_timed_new/2,
	remote_synchronous_timed_new_link/2, construct/2, delete/1 ]).



% Method declarations.
-define( wooper_method_export, getName/1, setName/2, display/1, toString/1 ).



% Helper functions:
-export([ send/2, send_from_test/2, send_standalone/2, get_current_tick/1,
	get_channel_name_for_priority/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Trace.Emitter").

% For trace_aggregator_name:
-include("class_TraceEmitter.hrl"). 

% For DefaultMessageCategorization:
-include("class_TraceAggregator.hrl").


-define(LogPrefix,"[Trace Emitter]").



% Constructs a new Trace emitter.
% EmitterName is the name of this trace emitter, ex: 'MyObject-16'.
% EmitterCategorization is the categorization of this emitter, ex:
% 'ABaseClass.AMotherClass.[..].MyClass'.
construct(State,?wooper_construct_parameters) ->

	%io:format( "~s Creating a trace emitter whose name is ~s, "
	%	"whose PID is ~w and whose categorization is ~s.~n", 
	%	[ ?LogPrefix, TraceEmitterName, self(), ?TraceEmitterCategorization ] ),
		
	% Retrieves the trace aggregator (false: do not launch it if not available,
	% otherwise the creation of multiple emitters would result in a race
	% condition that would lead to the creation of multiple aggregators):	
	AggregatorPid = class_TraceAggregator:getAggregator(false),
	?setAttributes( State, [ 
		{name,TraceEmitterName}, 
		{trace_categorization,?TraceEmitterCategorization},
		{trace_aggregator_pid, AggregatorPid} ] ).
	
	

	
% Overridden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	%io:format( "~s Deleting Trace Emitter.~n", [ ?LogPrefix ] ),
	% erlang:unlink() not used. 
	%io:format( "~s Trace Emitter deleted.~n", [ ?LogPrefix ] ).
	State.
	
	

% Methods section.


% Generic interface.	

% Returns the name of this trace emitter.
% (const request)	
getName(State) ->
	?wooper_return_state_result(State,?getAttr(name)).
	

% Sets the name of this trace emitter.
% (oneway)
setName(State,NewName) ->
	?wooper_return_state_only( ?setAttribute( State, name, NewName ) ).



% Displays the state in the console.
display(State) ->
	wooper_display_instance(State),
	?wooper_return_state_only( State ).


% Returns a textual description of this emitter.
toString(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).
	
	
		
	
% 'Static' methods (module functions):


% Implementation of functions used by trace macros.


% All informations available but the tick and the message categorization.
send( TraceType, [State, Message] ) ->
	send( TraceType, [ State, Message, ?DefaultMessageCategorization ] );

% All informations available but the tick, determining its availability.
send( TraceType, [State, Message, MessageCategorization ] ) ->
	case ?hasAttribute( State, current_time_offset ) of
	
		true ->
			% Should be an actor-like:
			case ?hasAttribute( State, starting_time ) of
			
				true ->
					send( TraceType, 
						[State, Message, MessageCategorization,
							get_current_tick(State)  ] );
				
				false ->
					% Execution not started: 'none' tick
					send( TraceType, 
						[State, Message, MessageCategorization, none ] )
			
			end;
			
		false ->
			% Not even an actor: 'unknown' tick
			send( TraceType, [State, Message, MessageCategorization,
				unknown ] )
	end;

% The function used to send all types of traces.
send( TraceType, 
		[State, Message, MessageCategorization, Tick ] ) ->
	% Follows the order of our trace format; oneway call:
	?getAttr(trace_aggregator_pid) ! { send, 
	%io:format( "PID = ~w, name = ~s, emitter categorization = ~s, "
	%	"tick = ~w, user time = ~s, location = ~s, "
	%	"message categorization = ~s, trace type = ~w, message = ~s ~n", 
		[ self(), ?getAttr(name), ?getAttr(trace_categorization), Tick, 
			current_time_to_string(), current_location(), MessageCategorization,
			get_priority_for(TraceType), Message ]
	% ).		 
	}.
	


% Sends all types of traces without requiring a class_TraceEmitter state.
% Uses default trace aggregator, supposed to be already available and 
% registered.
send_from_test( TraceType, [Message] ) ->
	send_from_test(TraceType, [Message, ?DefaultTestMessageCategorization]);
	
send_from_test( TraceType, [Message,MessageCategorization] ) ->
	% Follows the order of our trace format; oneway call:
	case global:whereis_name(?trace_aggregator_name) of
	
		undefined ->
			error_logger:info_msg( "class_TraceEmitter:send_from_test: "	
				"trace aggregator not found." ),	
			throw( trace_aggregator_not_found );
			
		AggregatorPid ->
			AggregatorPid ! { send, 
				[ self(), "Ceylan-trace-test", "Test", none, 
			current_time_to_string(), current_location(), MessageCategorization,
			get_priority_for(TraceType), Message ] }
	
	end.		



% Sends all types of traces without requiring a class_TraceEmitter state.
% Uses default trace aggregator, supposed to be already available and 
% registered.
send_standalone( TraceType, [Message] ) ->
	send_standalone(TraceType, [Message,
		?DefaultStandaloneMessageCategorization]);
	
send_standalone( TraceType, [Message,MessageCategorization] ) ->
	% Follows the order of our trace format; oneway call:
	case global:whereis_name(?trace_aggregator_name) of
	
		undefined ->
			error_logger:info_msg( "class_TraceEmitter:send_standalone: "	
				"trace aggregator not found." ),	
			throw( trace_aggregator_not_found );
			
		AggregatorPid ->
			AggregatorPid ! { send, 
				[ self(), "Ceylan", "Standalone", none, 
			current_time_to_string(), current_location(), MessageCategorization,
			get_priority_for(TraceType), Message ] }
	
	end.		





% Section for helper functions (not methods).


% Returns the current time and date as a string, with correct format.
% Example: "14/04/2008 04:41:24"
current_time_to_string() ->
	{Year,Month,Day} = date(),
	{Hour,Minute,Second} = time(),
	lists:flatten( io_lib:format( "~w/~w/~w ~B:~B:~B", 
		[ Day, Month, Year, Hour, Minute, Second ] ) ).
		
		
% Returns the current location of the trace emitter, with correct format.
current_location() ->
	case node() of
	
		nonode@nohost ->
			localhost;
			
		OtherName ->
		 	OtherName
	
	end.



% Returns the priority of specified trace type (i.e. fatal, error, etc.).
% Note: now that LogMX v1.3.2 and later only support 5 levels of detail 
% (stack/error, warning/warn, info, fine, finest/debug, i.e. no more trace),
% fatal and error messages have been put at the same priority level, and 
% Ceylan trace level has been kept, whereas others have been offset.
% See also: get_channel_name_for_priority/1.
% Corresponds to stack/error:
get_priority_for( fatal ) ->
	1 ;
	
% Corresponds to stack/error:
get_priority_for( error ) ->
	2 ;
	
% Corresponds to warning/warn:
get_priority_for( warning ) ->
	3 ;
	
% Corresponds to info:
get_priority_for( info ) ->
	4 ;
	
% Corresponds to fine:
get_priority_for( trace ) ->
	5 ;
	
% Corresponds to finest/debug:
get_priority_for( debug ) ->
	6.
	
	

% Returns the name of the trace channel corresponding to the trace priority.
% See also: get_priority_for/1
get_channel_name_for_priority(1) ->
	fatal;

get_channel_name_for_priority(2) ->
	error;
	
get_channel_name_for_priority(3) ->
	warning;
	
get_channel_name_for_priority(4) ->
	info;
	
get_channel_name_for_priority(5) ->
	trace;
	
get_channel_name_for_priority(6) ->
	debug.
	



% Returns the current (numerical) execution tick, in virtual gregorian seconds.
get_current_tick(State) ->
	%io:format( "get_current_tick called.~n" ),
	?getAttr(starting_time)	+ ?getAttr(current_time_offset).

