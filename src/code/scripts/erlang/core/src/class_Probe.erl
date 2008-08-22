% Probe base class.
% Aggregates a series of values for a series of ticks and generates an
% appropriate data file for gnuplot.
% Note: Ticks must arrive correctly ordered (increasing timestamps)
% A probe named 'Test probe' will result in the creation of two files:
%  - Test_probe.p, with the relevant gnuplot commands
%  - Test_probe.dat, with the probe data
% See class_Probe_test.erl
% See http://www.gnuplot.info/docs/gnuplot.html for graph generation.
% Needs gnuplot version 4.2 or higher, and an image viewer, eog (eye of gnome).
% See the executable_utils module.
-module(class_Probe).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Name,CurveNames,Title,XLabel,YLabel).

% Life-cycle related exported operators:
-define(wooper_construct_export, new/5, new_link/5, 
	synchronous_new/5, synchronous_new_link/5, construct/6, delete/1).


% Method declarations.
-define(wooper_method_export, setData/3, setPlotStyle/2, setFillStyle/2, 
	setKeyOptions/2, 
	generateReport/1, generateReport/2, getPlotCommand/1).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Probe.Generic").

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
	% (using default settings for graph rendering)
	StartState = ?setAttributes( TraceState, [ {curve_count,size(CurveNames)},
		{data_table,[]}, {trace_categorization,?TraceEmitterCategorization},
		{data_filename,file_utils:convert_to_filename(Name ++ ".dat")},
		{curve_names,CurveNames}, {title,Title}, 
		{key_options,"bmargin center horizontal"},
		{xlabel,XLabel}, {ylabel,YLabel},
		{xtic,"auto"}, {ytic,"auto"} , {yrange,"[]"},
		{plot_style,"linespoints"}, {fill_style,"empty"},
		{column_specifier,"1:"} ] ),

	?send_trace([ TraceState, "New probe created." ]),
		
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

	
	
% Sets the plot settings (ex: "histograms").
% (oneway)
setPlotStyle(State,"histograms") ->
	?wooper_return_state_only( ?setAttributes(State, [
		{plot_style,histograms}, {column_specifier,""} ] ) );

setPlotStyle(State,NewPlotStyle) ->
	?wooper_return_state_only( ?setAttribute(State,plot_style,NewPlotStyle) ).


% Sets the fill settings (ex: "solid 1.0 border -1").
% (oneway)
setFillStyle(State,NewFillStyle) ->
	?wooper_return_state_only( ?setAttribute(State,fill_style,NewFillStyle) ).


% Sets the key (legend) settings (ex: "inside left").
% (oneway)
setKeyOptions(State,NewOptions) ->
	?wooper_return_state_only( ?setAttribute(State,key_options,NewOptions) ).



% Generates a report for current state of this probe, and displays the result
% to the user.
% (request).	
generateReport(State) ->
	generateReport(State,true).


% Generates a report for current state of this probe.
%  - DisplayWanted: boolean telling whether the generated report will be
% displayed to the user (if true)
% (request).	
generateReport(State,DisplayWanted) ->
	?info([ "Generation of probe report requested." ]),

	generate_command_file(State),
	generate_data_file(State),

	Name = ?getAttr(name), 
	
	% Gnuplot might issue non-serious warnings. Generates a PNG:
	case os:cmd( "gnuplot " ++ get_command_filename( Name ) ) of
	
		[] ->
			ok;
			
		Message ->
			?warning([ io_lib:format( 
				"Report generation resulted in following output: ~s.",
				[Message] ) ]) 	

	end,
		
	case DisplayWanted of 
	
		true ->	
			executable_utils:display_png_file( get_report_filename(Name) );
	
		false ->
			ok
			
	end,	
	?wooper_return_state_result(State,probe_report_generated).


% Returns the Gnuplot command appropriate to render that probe output.
% Defines one plot curve per declared curve, with current plot settings. 
% (const request)
getPlotCommand(State) ->
	Prefix = io_lib:format( ", \"~s\"", [ ?getAttr(data_filename) ] ),	

	[_|CommandEnd] = make_plot_command( Prefix, State ),

	% tl to remove prefix head (','):
	?wooper_return_state_result( State, 
		io_lib:format( "plot ~s~s~n", [ tl(Prefix),CommandEnd ] ) ).


% Generic interface.	
		
	
% 'Static' methods (module functions):



% Section for helper functions (not methods).
	
	
% Generates the appropriate gnuplot command file.		
generate_command_file(State) ->
	Name = ?getAttr(name),
	Filename = get_command_filename( Name ),
	
	?trace([ io_lib:format( "Generating command file ~s.~n", [Filename] ) ]),
	{ok,File} = file:open( Filename, write ),

	io:format(File, "set autoscale~n",     []),
	io:format(File, "unset log~n",         []),
	io:format(File, "set grid~n",		   []),
	io:format(File, "set style data ~s~n", [ ?getAttr(plot_style) ] ),
	io:format(File, "set style fill ~s~n", [ ?getAttr(fill_style) ] ),
	io:format(File, "set key box ~s~n",    [ ?getAttr(key_options) ] ),
	io:format(File, "set xtic ~s~n",       [ ?getAttr(xtic) ] ),
	io:format(File, "set ytic ~s~n",       [ ?getAttr(ytic) ] ),
	io:format(File, "set yrange ~s~n",     [ ?getAttr(yrange) ] ),
	io:format(File, "set title \"~s\"~n",  [ ?getAttr(title) ]),
	io:format(File, "set xlabel \"~s\"~n", [ ?getAttr(xlabel) ]),
	io:format(File, "set ylabel \"~s\"~n", [ ?getAttr(ylabel) ]),
	% set terminal png *transparent* could be used as well:
	io:format(File, "set terminal png~n",  []),
	io:format(File, "set output \"~s\"~n", [ get_report_filename(Name) ]),

	% getPlotCommand may be overloaded (const method):
	% (returned state discarded)
	{wooper_result,_State,PlotCommand} = executeRequest(State,getPlotCommand),
	io:format(File, "~s", [ PlotCommand ]),
	file:close(File).


% Generates the appropriate file containing probe data.		
generate_data_file(State) ->
	{ok,File} = file:open( ?getAttr(data_filename), write ),
	write_rows( File, lists:reverse( ?getAttr(data_table) ) ),
	file:close(File).

	
% Returns the gnuplot command filename.
get_command_filename(Name) ->
	file_utils:convert_to_filename(Name ++ ".p").

	
% Returns the report filename.
get_report_filename(Name) ->
	file_utils:convert_to_filename(Name ++ ".png").
	
	
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
make_plot_command( Prefix, State ) ->
	Count = ?getAttr(curve_count)+1,
	CurveNames = ?getAttr(curve_names),
	make_plot_command( [], Count, Prefix, State,
		lists:reverse( tuple_to_list( CurveNames ) ) ).
	
	
make_plot_command(Acc,1,_Prefix,_State,_CurveNames) ->
	Acc;

make_plot_command( Acc, Count, Prefix, State, [CurveName|OtherNames] ) ->
	make_plot_command( 
		io_lib:format( "~s using ~s~B title \"~s\" with ~s",
			[ Prefix, ?getAttr(column_specifier), Count, CurveName,
				?getAttr(plot_style) ] ) ++ Acc, 
		Count-1, Prefix, State, OtherNames ).
		
