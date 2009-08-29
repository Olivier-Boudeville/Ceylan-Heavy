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


-module(class_TraceAggregator).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).



% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters, TraceFilename, TraceType, IsPrivate ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3, 
	synchronous_new/3, synchronous_new_link/3,
	synchronous_timed_new/3, synchronous_timed_new_link/3,
	remote_new/4, remote_new_link/4, remote_synchronous_new/4,
	remote_synchronous_new_link/4, remote_synchronous_timed_new/4,
	remote_synchronous_timed_new_link/4, construct/4, delete/1 ).



% Method declarations.
-define(wooper_method_export, send/10, addTraceListener/2, 
	removeTraceListener/2 ).


% Static method declarations (to be directly called from module):
-export([ create/1, getAggregator/1, remove/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For registration name:
-include("class_TraceAggregator.hrl").


-define(LogPrefix,"[Trace Aggregator]").


% Use global:registered_names() to check aggregator presence.


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).



% Implementation Notes.
%
% The aggregator could store per-emitter constant settings (ex: emitter name
% and categorization) instead of having them sent to it each time.
%



% Constructs a new trace aggregator.
%  - TraceFilename is the name of the file where traces should be written to.
%  - TraceType is either 'log_mx_traces', '{text_traces,text_only}' or
% '{text_traces,pdf}', depending whether LogMX should be used to browse the
% execution traces, or just a text viewer.
%  - IsPrivate tells whether this trace aggregator will be privately held
% (hence should not be registered in naming service) or if it is a (registered) 
% singleton.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes (none), then this class-specific actions:

	PrivateState = case IsPrivate of 
	
		true ->
			io:format( "~n~s Creating a private trace aggregator, "
				"whose PID is ~w.~n", [ ?LogPrefix, self() ] ),
			?setAttribute(State,is_private,true);
		
		false ->
			io:format( "~n~s Creating the trace aggregator, whose PID is ~w.~n",
				[ ?LogPrefix, self() ] ),
				
			basic_utils:register_as( ?trace_aggregator_name, local_and_global ),

			?setAttribute(State,is_private,false)

	end,
	
	{ok,File} = file:open( TraceFilename, [write] ),

	SetState = ?setAttributes( PrivateState, [ 
		{trace_filename,TraceFilename}, 
		{trace_file,File},
		{trace_type,TraceType},
		{trace_listeners,[]} ] ),
	io:format( "~s Trace aggregator created.~n", [ ?LogPrefix ] ),
	manage_trace_header(SetState).

	
	
% Overridden destructor.
delete(State) ->
	io:format( "~s Deleting trace aggregator.~n", [ ?LogPrefix ] ),

	% Class-specific actions:
	case ?getAttr(is_private) of
	
		true ->
			ok;
			
		false ->
			basic_utils:unregister( ?trace_aggregator_name, local_and_global )
			
	end,
	
	FooterState = manage_trace_footer(State),
	
	ok = file:close( ?getAttr(trace_file) ),
	
	case ?getAttr(trace_type) of 
	
		log_mx_traces ->
			ok;
			
		{text_traces,text_only} ->
			ok;
			
		{text_traces,pdf} ->
		
			io:format( "~s Trace aggregator: generating PDF trace report.~n",
				[ ?LogPrefix ] ),
				
			PdfTargetFilename = file_utils:replace_extension( 
				?getAttr(trace_filename), ?TraceExtension, ".pdf" ),
				
			Command = "if make " ++ PdfTargetFilename 
				++ "; then echo ok; else echo error; fi",
				 
			case os:cmd( Command ) of

				"ok\n" ->	
				
					io:format( "~s Trace aggregator: "
						"displaying PDF trace report.~n", [ ?LogPrefix ] ),

					os:cmd("evince " ++ PdfTargetFilename ) ;
				
				_Error ->
					io:format( "~s Trace aggregator: "
						"generation of PDF from ~s failed.~n", 
						[ ?LogPrefix, ?getAttr(trace_filename) ] )	
						
			end			
					
	end,

	% Then call the direct mother class counterparts: (none)
	
	io:format( "~s Trace aggregator deleted.~n", [ ?LogPrefix ] ),
	
	% Allow chaining:
	FooterState.
	
	

	
	
% Methods section.


% Sends a full trace to this aggregator to have it stored.
% The nine fields correspond to the ones defined in our trace format.
% (const oneway)
send( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ) ->
			
	Trace = format_trace_for( ?getAttr(trace_type), 
		{ TraceEmitterPid, TraceEmitterName,
		TraceEmitterCategorization, Tick, Time, Location, 
		MessageCategorization, Priority, Message } ),
				
	io:format( ?getAttr(trace_file), "~s", [Trace] ),
	
	Listeners = ?getAttr(trace_listeners),
	case length(Listeners) of 
		
		0 ->
			ok;
			
		_Other ->	
			BinTrace = list_to_binary(Trace),
			lists:foreach( 
				fun(Listener) ->	
		
					Listener ! {addTrace,BinTrace}
				
				end,
				Listeners )
			
	end,		 
	?wooper_return_state_only( State ).



% Adds specified trace listener to this aggregator.
% (oneway)
addTraceListener(State,ListenerPid) ->
	% Better log this events directly in the traces:
	NewState = case ?getAttr(trace_type) of

		log_mx_traces ->
			?emit_info([ io_lib:format( 
				"Trace aggregator adding trace listener ~w, "
				"and sending it previous traces.~n", [ ListenerPid ] ) ]),
			% Transfer file:
			TraceFilename = ?getAttr(trace_filename),
			Bin = file_utils:file_to_zipped_term( TraceFilename ),
			ListenerPid ! {trace_zip,Bin,TraceFilename},
			?appendToAttribute( State, trace_listeners,	ListenerPid);
				
		OtherTraceType ->
			Message = io_lib:format( 
				"Trace aggregator not adding trace listener ~w, "
				"as it requires LogMX traces, whereas the current trace "
				"type is ~w.~n", [ ListenerPid, OtherTraceType ] ),
			io:format( "Warning: " ++ Message ),	
			?emit_warning([ Message ]),
			ListenerPid ! {trace_zip,incompatible_trace_type},
			State
				
	end,
	?wooper_return_state_only( NewState ).
		

% Removes specified trace listener from this aggregator.
% (oneway)
removeTraceListener(State,ListenerPid) ->
	io:format( "~s Removing trace listener ~w.~n", 
		[ ?LogPrefix, ListenerPid ] ),
	UnregisterState = ?deleteFromAttribute(State,trace_listeners,ListenerPid),
	?wooper_return_state_only( UnregisterState ).
	



% 'Static' methods (module functions):
	
	
% Creates the trace aggregator asynchronously, with default settings.
% (static)	
create( UseSynchronousNew ) ->
	create( UseSynchronousNew, _TraceType = log_mx_traces ).
	
	
	
% Creates the trace aggregator asynchronously, using specified trace type.
% (static)	
create( _UseSynchronousNew = false, TraceType ) ->
	new( ?trace_aggregator_filename, TraceType, _IsPrivate = false  );

% Creates the trace aggregator synchronously, using specified trace type.
% (static)	
create( _UseSynchronousNew = true, TraceType ) ->
	% Trace filename, isPrivate:
	synchronous_new( ?trace_aggregator_filename, TraceType, 
		_IsPrivate = false ).




% Returns the Pid of the current trace aggregator.
% Parameter is a boolean that tells whether the aggregator should be created
% if not available (if true) or if this method should just return a failure
% notification (if false).
% Note: to avoid race conditions between concurrent calls to this static 
% method (ex: due to trace emitter instances), a execution might start with
% a call to this method with a blocking wait until the aggregator pops up in
% registry services.
% Waits a bit before giving up: useful when client and aggregator processes are
% launched almost simultaneously.
% (static)
getAggregator(CreateIfNotAvailable) ->
	% Only dealing with registered managers (instead of using directly
	% their PID) allows to be sure only one instance (singleton) is
	% being used, to avoid the case of two managers being launched at
	% the same time (the second will then terminate immediately).
	
	% If launching multiple trace emitters in a row, first emitter may 
	% trigger the launch of trace aggregator, but second emitter might do the
	% same if the aggregator is still being initialized: 
	case basic_utils:wait_for_global_registration_of( ?trace_aggregator_name )
			of
	
		AggregatorPid when is_pid(AggregatorPid) ->
			% Already available, return its PID:
			AggregatorPid;
		
		{registration_waiting_timeout,?trace_aggregator_name} ->
		
			case CreateIfNotAvailable of 
			
				true ->
				
					% Not available, launch it synchronously (with default
					% settings):
					create(true),
					case basic_utils:wait_for_global_registration_of(
							?trace_aggregator_name ) of
	
						LaunchedAggregatorPid 
								when is_pid(LaunchedAggregatorPid) ->
							LaunchedAggregatorPid;	

						{registration_waiting_timeout,?trace_aggregator_name} ->
							error_logger:error_msg( 
								"class_TraceAggregator:getAggregator "
								"unable to launch successfully "
								"the aggregator.~n" ),
						trace_aggregator_launch_failed
						
					end;
					
				false ->			
					% Not available and not to be started:
					trace_aggregator_not_found
			
			end
	end.		
			


% Deletes the trace aggregator.
% (static)	
remove() ->

	case global:whereis_name( ?trace_aggregator_name ) of
	
		undefined ->
			trace_aggregator_not_found;
			
		TraceAggregatorPid ->
			TraceAggregatorPid ! delete
			
	end.




% Some defines.


% Columns in text traces have fixed width, in characters (total: 110).
% In this mode, by default, emitter categorization, location and message
% categorization are not written. 


% For Pid (ex: locally, <0.33.0>):
-define(pid_width,8).


% For EmitterName (ex: "First soda machine"):
-define(emitter_name_width,12).


% For EmitterCategorization (ex: "TimeManagement"):
%-define(emitter_categorization_width,12).
-define(emitter_categorization_width,0).


% For Tick (ex: unknown, 3169899360000):
-define(tick_width,14).


% For Time (ex: "18/6/2009 16:32:14"):
-define(time_width,10).


% For Location (ex: "soda_deterministic_integration_run@a_example.org"):
%-define(location_width,12).
-define(location_width,0).


% For MessageCategorization (ex: "Execution.Uncategorized"):
%-define(message_categorization_width,4).
-define(message_categorization_width,0).


% For Priority (ex: warning):
-define(priority_width,7).


% For Message:
-define(message_width,45).




% Helper functions.



% Takes care of any header in the trace header.
% Returns an updated state.
% (helper function)
manage_trace_header(State) ->

	case ?getAttr(trace_type) of
	
		log_mx_traces ->
			State;
			
		{text_traces,_TargetFormat} ->
		
			io:format( ?getAttr(trace_file), 
				"~n==============================", [] ),
				
			io:format( ?getAttr(trace_file), 
				"~nCeylan Execution Trace Report", [] ),
				
			io:format( ?getAttr(trace_file), 
				"~n==============================~n~n~n", [] ),
						
			io:format( ?getAttr(trace_file), "~n.. _table:", [] ),
			
			io:format( ?getAttr(trace_file), 
				"~n.. contents:: Table of Contents~n~n", [] ),

			io:format( ?getAttr(trace_file), "~nExecution Context", [] ),
			io:format( ?getAttr(trace_file), "~n------------------~n~n", [] ),

			io:format( ?getAttr(trace_file), 
				"Report generated on ~s, from trace file ``~s``, "
				"on host ``~s``.~n~n",
				[ basic_utils:get_textual_timestamp(), ?getAttr(trace_filename),
				net_adm:localhost() ] ),

			io:format( ?getAttr(trace_file), "~nTrace Begin", [] ),
			io:format( ?getAttr(trace_file), "~n-----------~n~n", [] ),
			
			% Prints array header:
			io:format( ?getAttr(trace_file), get_row_separator(), [] ),
			PidLines = basic_utils:format_text_for_width( 
				"Pid of Trace Emitter", ?pid_width ),
		
			EmitterNameLines = basic_utils:format_text_for_width( 
				"Emitter Name", ?emitter_name_width ),
		
			TickLines = basic_utils:format_text_for_width(
				"Execution Tick", ?tick_width ),
		
			TimeLines = basic_utils:format_text_for_width( "User Time",
				?time_width ),
		
			PriorityLines = basic_utils:format_text_for_width( 
				"Trace Type", ?priority_width ),
	
			MessageLines = basic_utils:format_text_for_width( 
				"Trace Message", ?message_width ),
	
			HeaderLine = format_linesets( PidLines, EmitterNameLines, 
				TickLines, TimeLines, PriorityLines, MessageLines ) ,
				
			io:format( ?getAttr(trace_file), HeaderLine, [] ),
			io:format( ?getAttr(trace_file), get_row_separator($=), [] ),
			State
				
	end.
	


% Takes care of any header in the trace header.
% Returns an updated state.
% (helper function)
manage_trace_footer(State) ->
	case ?getAttr(trace_type) of
	
		log_mx_traces ->
			State;
			
		{text_traces,_TargetFormat} ->
			io:format( ?getAttr(trace_file), "~nTrace End", [] ),
			io:format( ?getAttr(trace_file), "~n---------~n~n", [] ),
			io:format( ?getAttr(trace_file), 
				"~n~nEnd of execution traces.~n", [] ),
			io:format( ?getAttr(trace_file), 
				"~nBack to the table_ of contents "
				"and to the beginning of traces.", [] ),
			State
				
	end.



% Returns the typical separator between array rows.
get_row_separator() ->
	get_row_separator($-).
	
	
% Returns the typical separator between array rows, with specified dash element
% to represent horizontal lines.
get_row_separator(DashType) ->
	[$+] ++ string:chars(DashType,?pid_width) 
		++ [$+] ++ string:chars(DashType,?emitter_name_width)
		++ [$+] ++ string:chars(DashType,?tick_width)
		++ [$+] ++ string:chars(DashType,?time_width)
		++ [$+] ++ string:chars(DashType,?priority_width)
		++ [$+] ++ string:chars(DashType,?message_width) 
		++ [$+] ++ "\n".
	


% Formats specified trace according to specified trace type.
format_trace_for( log_mx_traces, {TraceEmitterPid,
		TraceEmitterName, TraceEmitterCategorization, Tick, Time, Location,
		MessageCategorization, Priority, Message} ) ->
	lists:flatten( io_lib:format( "~w|~s|~s|~w|~s|~s|~s|~w|~s~n", 
		[ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ] ) );

format_trace_for( {text_traces,_TargetFormat}, {TraceEmitterPid,
		TraceEmitterName, _TraceEmitterCategorization, Tick, Time, _Location,
		_MessageCategorization, Priority, Message} ) ->
		
	% Not output here:
	%  - TraceEmitterCategorization
	%  - Location
	%  - MessageCategorization
	
	PidLines = basic_utils:format_text_for_width( 
		io_lib:format( "~w", [TraceEmitterPid] ), ?pid_width ),
		
	EmitterNameLines = basic_utils:format_text_for_width( 
		io_lib:format( "~s", [TraceEmitterName] ), ?emitter_name_width ),
		
	% Can be a tick or an atom like 'unknown':	
	TickLines = basic_utils:format_text_for_width( 
		io_lib:format( "~p", [Tick] ), ?tick_width ),
		
	TimeLines = basic_utils:format_text_for_width( 
		io_lib:format( "~s", [Time] ), ?time_width ),
		
	PriorityLines = basic_utils:format_text_for_width( 
		io_lib:format( "~w", [
			class_TraceEmitter:get_channel_name_for_priority(Priority) ]),
		?priority_width ),
	
	MessageLines = basic_utils:format_text_for_width( 
		io_lib:format( "~s", [Message] ), ?message_width ),
	
	format_linesets( PidLines, EmitterNameLines, TickLines, TimeLines,
		PriorityLines, MessageLines ) ++ get_row_separator().
		


% Formats specified list of linesets.
format_linesets( PidLines, EmitterNameLines, TickLines, TimeLines,
		PriorityLines, MessageLines ) ->
		
	Columns = [ PidLines, EmitterNameLines, TickLines, TimeLines, PriorityLines,
		MessageLines ],
	
	TotalLineCount = lists:max( [ length(L) || L <- Columns ] ),
	
	ColumnsPairs = [ 
		{PidLines,?pid_width}, 
		{EmitterNameLines,?emitter_name_width},
		{TickLines,?tick_width}, 
		{TimeLines,?time_width},
		{PriorityLines,?priority_width}, 
		{MessageLines,?message_width} ], 
	
	%io:format( "Column pairs:~n~p~n", [ColumnsPairs] ),
	
	FullLines = format_full_lines( ColumnsPairs, _Acc = [], TotalLineCount, 
		_Res = [], _CurrentLine = "" ),
		
	string:join( FullLines, "\n" ).
	
	
	
% Returns a list of full lines, made from the lines of each column. 
% Here we finished to handle all lines (none remaining):	
format_full_lines( _Rows, [], 0, Res, CurrentLine ) ->
	lists:reverse( [CurrentLine|Res] ) ;
	
% Here we arrived at the end of a global line, preparing for next one:	
format_full_lines( [], Acc, RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( lists:reverse(Acc), [], RemainingLineCount-1, 
		[CurrentLine++ "|"|Res], "" );

% Here the corresponding column has no more content, just filling with spaces:
format_full_lines( [{[],Width}|ColumnPairs], Acc, RemainingLineCount, Res, 
		CurrentLine ) ->
	format_full_lines( ColumnPairs, [ {[],Width} | Acc ], RemainingLineCount,
		Res, CurrentLine ++ "|" ++ string:chars($\ ,Width) );
	
% Here the corresponding column has content, just adding it:
format_full_lines( [{[Line|OtherLines],Width}|ColumnPairs], Acc,
		RemainingLineCount, Res, CurrentLine ) ->		
	format_full_lines( ColumnPairs, [ {OtherLines,Width} | Acc ],
		RemainingLineCount, Res, CurrentLine ++ "|" ++ Line ).

