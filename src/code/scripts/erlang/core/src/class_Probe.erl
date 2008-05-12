% Probe base class.
% Aggregates a series of values for a series of ticks and generates an
% appropriate data file for gnuplot.
% Note: Ticks must arrive correctly ordered (increasing timestamps)
% A probe named 'Test probe' will result in the creation of two files:
%  - Test_probe.p, with the relevant gnuplot commands
%  - Test_probe.plotsim, with the probe data
% See class_Probe_test.erl
% See http://www.gnuplot.info/docs/gnuplot.html for graph generation.
% Needs gnuplot version 4.2 or higher, and uses eog (eye of gnome).
-module(class_Probe).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Name,CurveNames,Title,XLabel,YLabel).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/5,new_link/5,
	synchronous_new/5,synchronous_new_link/5,construct/6,delete/1).


% Method declarations.
-define(wooper_method_export,setData/3,generateReport/1,display/1,toString/1).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Probe").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Constructs a new Probe.
%  - Name is the name of this probe, and will be used for the generated data and
% command files
%  - CurveNames is a tuple containing the names of each curve to be drawn (hence
% the probe will expect receiving data in the form {Tick,{V1,V2,..}}).
%  - Title will be the graph title
%  - XLabel will be the label of the abscissa axis
%  - YLabel will be the label of the ordinate axis
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct(State,Name),

	% Then the class-specific actions:
	
	% data_table is not a hashtable because gnuplot wants the rows to be
	% ordered: 
	StartState = ?setAttributes( TraceState, [ {curve_count,size(CurveNames)},
		{data_table,[]}, {trace_categorization,?TraceEmitterCategorization},
		{data_filename,utils:convert_to_filename(Name ++ ".plotsim")} ] ),

	?send_trace([ TraceState, "Creating a new probe." ]),
		
	generate_command_file(Name,CurveNames,Title,XLabel,YLabel,StartState),
	StartState.	

	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?trace([ "Deleting probe." ]),
	% erlang:unlink() not used. 
	?debug([ "Probe deleted." ]),
	
	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	

	
	

% Methods section.

% Registers specified samples.
% (oneway).
setData(State,Tick,Samples) ->
	?debug([ io_lib:format( "setData called for tick ~B with samples ~w.",
		[Tick,Samples]) ]),
	ExpectedCount = ?getAttr(curve_count),
	% Integrity check (pattern-matching):	
	ExpectedCount = size(Samples), 	
	?wooper_return_state_only( ?appendToAttribute( State, data_table,
		{Tick,Samples} ) ).
	

% Generates a report for current state of this probe.
% (request).	
generateReport(State) ->
	?info([ "Generation of probe report requested." ]),
		
	{ok,File} = file:open( ?getAttr(data_filename), write ),
	write_rows( File, lists:reverse( ?getAttr(data_table) ) ),
	file:close(File),
	
	Name = ?getAttr(name), 
	
	% Gnuplot might issue non-serious warnings:
	os:cmd( "gnuplot " ++ get_command_filename(Name) ),
	
	% Viewer is 'eye of gnome' here: 
	os:cmd( "eog "     ++ get_report_filename(Name) ++ " &" ),
	
	?wooper_return_state_result(State,report_generated).


% Generic interface.	
	
% Displays the state in the console.
display(State) ->
	wooper_display_instance(State),
	?wooper_return_state_only( State ).


% Returns a textual description of this manager.
toString(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).
	
	
		
	
% 'Static' methods (module functions):



% Section for helper functions (not methods).
	
	
% Generates the appropriate gnuplot command file.		
generate_command_file(Name,CurveNames,Title,XLabel,YLabel,State) ->
	Filename = get_command_filename(Name),
	?trace([ io_lib:format( "Generating command file ~s.~n", [Filename] ) ]),
	{ok,File} = file:open( Filename, write ),

	io:format(File, "set autoscale~n",     []),
	io:format(File, "unset log~n",         []),
	io:format(File, "set xtic auto~n",     []),
	io:format(File, "set ytic auto~n",	   []),
	io:format(File, "set grid~n",		   []),
	%io:format(File, "set key top left~n", []),
	io:format(File, "set key bmargin center horizontal~n", []),
	io:format(File, "set key box~n",		   []),
	io:format(File, "set title \"~s\"~n",  [Title]),
	io:format(File, "set xlabel \"~s\"~n", [XLabel]),
	io:format(File, "set ylabel \"~s\"~n", [YLabel]),
	% set terminal png *transparent* could be used as well:
	io:format(File, "set terminal png font \"arial\" 8~n",  []),
	io:format(File, "set output \"~s\"~n", [get_report_filename(Name)]),
	Prefix = io_lib:format( ", \"~s\"", [ ?getAttr(data_filename) ]),	

	[_|CommandEnd] = make_using_command( ?getAttr(curve_count)+1,
		Prefix,CurveNames),

	io:format(File, "plot ~s~s~n", [ tl(Prefix),CommandEnd ]),
	file:close(File).
	
	
% Returns the gnuplot command filename.
get_command_filename(Name) ->
	utils:convert_to_filename(Name ++ ".p").

	
% Returns the report filename.
get_report_filename(Name) ->
	utils:convert_to_filename(Name ++ ".png").
	
	
% Writes the rows of the data table into specified file.
write_rows(_File,[]) ->
	ok;
	
write_rows( File, [{Tick,DataTuple}|T]) ->
	io:format( File, "~B ~s~n", [ round(Tick), make_data_row(DataTuple) ] ),
	write_rows(File,T).
			 
	 
% Returns a string contained the samples.	
make_data_row(DataTuple) ->	
	make_data_row( erlang:tuple_to_list(DataTuple), [] ).
	
	
% Transforms the list into a string.
make_data_row( [], Acc ) ->
	 lists:reverse(Acc);
	 
make_data_row( [H|T], Acc ) ->
	make_data_row(T, io_lib:format( "~w ", [H]) ++ Acc ).


% Generates the appropriate gnuplot 'using' command.
make_using_command( Count, Prefix, CurveNames ) ->
	make_using_command( [], Count, Prefix,
		lists:reverse( tuple_to_list(CurveNames ))).
	
	
make_using_command(Acc,1,_,_) ->
	Acc;

make_using_command( Acc, Count, Prefix, [CurveName|OtherNames] ) ->
	make_using_command( 
		io_lib:format( "~s using 1:~B title \"~s\" with linespoints",
			[ Prefix, Count, CurveName ] ) ++ Acc, 
		Count-1, Prefix, OtherNames ).
		
