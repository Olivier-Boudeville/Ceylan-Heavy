% The purpose of this process is, on a per-node basis, to create and notably
% to serve to instances the virtual table corresponding to the actual class
% they are corresponding to.
% This way each virtual table is computed only once per node, and no significant
% per-instance memory is used for the virtual table : all the instances of a 
% given class just refer to a common virtual table stored by this manager.
-module(wooper_class_manager).

-export([start/0]).

% For WooperClassManagerName :
%-include("wooper_class_root.hrl").

% Currently duplicated :
-define(WooperClassManagerName,wooper_class_manager).

% Approximate average method count for a given class, including inherited ones.
% (ideally should be slightly above the maximum number of actual methods)
-define(WooperMethodCountUpperBound,5).


% Approximate average class count for the program.
% (ideally should be slightly above the maximum number of actual classes being
% instanciated)
-define(ClassCountUpperBound,200).


% Starts a new blank class manager.
start() ->
	io:format("Starting WOOPER class manager.~n"),
	% Two instances being created nearly at the same time might trigger the
	% creation of two class managers, if the second instance detects no manager
	% is registered while the first manager is created but not registered yet.
	case catch register( ?WooperClassManagerName, self() ) of
	
		true -> 
			loop( hashtable:new( ?ClassCountUpperBound ) );
		
		% A manager is already registered, let it be the only one:
		{ 'EXIT', {badarg,_} } ->
			io:format( "wooper_class_manager : already a manager available." ),
			ok
		
	end.


% Manager main loop, serves virtual tables on request (mostly on instances
% creation).	
loop(Tables) ->
	io:format("Class manager storing ~B table(s).~n",
		[ hashtable:getEntryCount(Tables) ] ),
	receive
	
		{ get_table, Module, Pid } ->
			{ NewTables, TargetTable } = get_virtual_table_for(Module,Tables),
			Pid ! { virtual_table, Module, TargetTable},
			loop( NewTables );
		
		stop ->
			io:format( "Class manager stopping.~n" )	
			
	end.
	

% Look-up specified table. If found, returns it immediately, otherwise 
% constructs it, stores the result and returns it as well.	
% Virtual tables are stored in a hashtable.
% Returns a pair formed of the new set of virtual tables and of the requested
% table.
get_virtual_table_for(Module,Tables) ->
	case hashtable:lookupEntry(Module,Tables) of 
	
		undefined ->
			% Time to create this virtual table and to store it :
			io:format( "Creating a virtual table for module ~s.~n", 
				[ Module ] ),
			ModuleTable = create_method_table_for( Module ),
			{ hashtable:addEntry( Module, ModuleTable, Tables ), ModuleTable };
				
			
		{value,Table} ->
			% Cache hit, no change in internal data :
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
	hashtable:merge(HashTable,create_method_table_for(Module)).


% Returns a Hashtable appropriate for method look-up, for the specified module.
% Constructors (construct) could be ignored.
create_local_method_table_for(Module) ->
	lists:foldl(
		fun(FunNameArityPair,HashTable) ->
			hashtable:addEntry(FunNameArityPair,Module,HashTable)
		end,
		hashtable:new(?WooperMethodCountUpperBound),
		Module:module_info(exports)).

