% Module of the WOOPER class manager.
-module(wooper_class_manager).

% The purpose of this process is, on a per-node basis, to create and notably
% to serve to instances the virtual table corresponding to the actual class
% they are corresponding to.

% This way each virtual table is computed only once per node, and no significant
% per-instance memory is used for the virtual table: all the instances of a 
% given class just refer to a common virtual table stored by this manager.


% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date: Friday, July 12, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL, see:
% http://ceylan.sourceforge.net/main/documentation/wooper/index.html#license



-export([ start/1, ping/1 ]).


% For WooperClassManagerName:
-include("wooper_class_manager.hrl").


% Approximate average method count for a given class, including inherited ones.
% (ideally should be slightly above the maximum number of actual methods)
-define(WooperMethodCountUpperBound,32).


% Approximate average class count for the program.
% (ideally should be slightly above the maximum number of actual classes being
% instanciated)
-define(WooperClassCountUpperBound,128).


% Comment/uncomment to respectively disable and enable debug mode:
%-define(debug,).
 
-define(Log_prefix, "[WOOPER Class manager] ").
 
-ifdef(debug).

	
	display_state(Tables) ->
		error_logger:info_msg( ?Log_prefix "Storing now ~B table(s).~n",
			[ hashtable:getEntryCount(Tables) ] ).

	display_table_creation(Module) ->
		error_logger:info_msg( ?Log_prefix "Creating a virtual table "
			"for module ~s.~n", [ Module ] ).
			
	display(String) ->
		error_logger:info_msg( ?Log_prefix "~s.~n", [ String ] ).
			
-else.

	display_state(_) ->
		ok.

	display_table_creation(_) ->
		ok.
		
	display(_) ->
		ok.
		
-endif.		



% Starts a new blank class manager.
start(ClientPID) ->

	display( io_lib:format( "Starting on node ~s (PID: ~w).~n", 
		[ node(), self() ] ) ),

	% Two first instances being created nearly at the same time might trigger
	% the creation of two class managers, if the second instance detects no
	% manager is registered while the first manager is created but not
	% registered yet. That would result in superflous class managers. Up to one
	% should exist.
	% Note: as the register call cannot be performed at once (not an atomic
	% operation), there must remain a tiny window for a race condition to
	% happen). 
	% Local registration only, as we want the instances to benefit from a 
	% local direct reference to the same method table, rather waste memory
	% with one copy of the table per instance.
	% In a distributed context, there should be exactly one class manager
	% per node.
	case catch register( ?WooperClassManagerName, self() ) of
	
		true ->
			ClientPID ! class_manager_registered,
			loop( hashtable:new( ?WooperClassCountUpperBound ) );
		
		% A manager is already registered, let it be the only one and stop:
		{ 'EXIT', {badarg,_} } ->
			display( ?Log_prefix "Already a manager available, terminating" ),
			% Let's notify the client nevertheless:
			ClientPID ! class_manager_registered
			% The instances should use the first manager only.
			% (no looping performed, terminating this second manager).
		
	end.



% Manager main loop, serves virtual tables on request (mostly on instances
% creation).	
loop(Tables) ->
	display_state(Tables),
	receive
	
		{ get_table, Module, Pid } ->
			{ NewTables, TargetTable } = get_virtual_table_for(Module,Tables),
			Pid ! { virtual_table, Module, TargetTable },
			loop( NewTables );
		
		display ->
			error_logger:info_msg( ?Log_prefix "Internal state is: ~s~n.",
				[ hashtable:toString(Tables) ] ),
			loop( Tables );
				
		stop ->
			unregister( ?WooperClassManagerName ),
			display( "Stopped on request" )	
			
	end.
	

% Look-up specified table. If found, returns it immediately, otherwise 
% constructs it, stores the result and returns it as well.	
% Virtual tables are stored in a hashtable.
% Returns a pair formed of the new set of virtual tables and of the requested
% table.
get_virtual_table_for(Module,Tables) ->
	case hashtable:lookupEntry(Module,Tables) of 
	
		{hashtable_key_not_found,_Key} ->
			% Time to create this virtual table and to store it:
			display_table_creation( Module ),
			ModuleTable = create_method_table_for( Module ),
			% Here the table could be patched with delete/1, if defined.
			{hashtable:addEntry( Module, ModuleTable, Tables ), ModuleTable };
				
			
		{value,Table} ->
			% Cache hit, no change in internal data:
			{Tables, Table}
			
	end.


% Creates recursively (indirectly thanks to 'update_method_table_with') the
% virtual table corresponding to specified module.
create_method_table_for(TargetModule) ->
	lists:foldl(
		fun(Module,HashTable) ->
			update_method_table_with(Module,HashTable)
		end,		
		create_local_method_table_for(TargetModule),
		apply(TargetModule,get_superclasses,[])).


% Updates specified virtual table with the method of specified module
% (i.e. precomputes the virtual table for the related class)
% In case of key collision, the values specified in HashTable have
% priority over the ones relative to Module. Hence methods redefined in child
% classes are selected, rather than the ones of the mother class.
update_method_table_with(Module,HashTable) ->
	hashtable:merge( HashTable, create_method_table_for(Module) ).


% Tells whether the function Name/Arity should be registered into the method
% virtual table.
select_function(_,0)                          -> false ;
select_function(new,_)                        -> false ;
select_function(construct,_)                  -> false ;
select_function(delete,1)                     -> false ;
select_function(wooper_construct_and_run,_)   -> false ;
select_function(wooper_debug_listen,_)        -> false ;
select_function(module_info,1)                -> false ;
select_function(_,_)                          -> true.


% Returns a Hashtable appropriate for method look-up, for the specified module.
create_local_method_table_for(Module) ->
	% Filter-out functions that should not be callable via RMI:
	lists:foldl(
		% Filter-out functions that should not be callable via RMI:
		fun({Name,Arity}, HashTable) ->
			case select_function(Name,Arity) of 
				
				true -> 
					hashtable:addEntry({Name,Arity},Module,HashTable);
				
				false -> 	
					HashTable
					
			end
		end,
		hashtable:new(?WooperMethodCountUpperBound),
		Module:module_info(exports)).


% ping specified WOOPER instance, returns pong if it could be successfully
% pinged, otherwise returns pang.
ping(Target) when is_pid(Target) ->
	Target ! {ping, self()},
	receive
	
		{pong,Target} ->
			pong
			
		after 500 ->
			pang
			
	end;
	
ping(Target) when is_atom(Target) ->
	case global:whereis_name( Target ) of 
		
		undefined -> 
			case whereis( Target ) of 	
				undefined ->
					pang;
				
				Pid ->
					ping(Pid)
			end;	
				
		Pid ->
			ping(Pid)
			
	end.

