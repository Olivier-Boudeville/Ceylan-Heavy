% Generic hash table implementation.
% See hashtable_test.erl for the corresponding test.

% An hashtable is basically a tuple whose size (number of elements) is the
% number of buckets in the hashtable. Each element of the tuple is a list
% containing key/value pairs.

-module(hashtable).
% Directly depends on : utils module.

% Creation date: July 2, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).
% Heavily inspired of the tupleStore example from 
% 'Concurrent Programming in Erlang', section 9.8.

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


% The hashtable is implemented thanks to a tuple whose size is the number of
% buckets specified at the hashtable creation.
% Each tuple element (hence each bucket) is a list of key/value pairs.

% Maybe the ets module could/should be used instead.


-export([new/0,new/1,addEntry/3,addEntries/2, 
	removeEntry/2,lookupEntry/2,hasEntry/2,
	getEntry/2,addToEntry/3,substractFromEntry/3,toggleEntry/2,
	appendToEntry/3,deleteFromEntry/3,popFromEntry/2,
	enumerate/1,getEntryCount/1,merge/2,toString/1,display/1]).


% The default number of hash buckets :
-define(DefaultNumberOfBuckets,256).



% Creates a new empty hash table with the default number of buckets.
new() ->
	new(?DefaultNumberOfBuckets).


% Returns a new empty hash table with specified number of buckets.
% For efficient access, there should be more buckets than entries.  
new(NumberOfBuckets) ->
	createTuple(NumberOfBuckets,[]).


% Adds specified key/value pair into the specified hash table.
% If there is already a pair with this key, then its previous value
% will be replaced by the specified one.
addEntry(Key,Value,HashTable) ->
	KeyIndex=erlang:phash2(Key,size(HashTable))+1,
	% Retrieve appropriate tuple slot :
	PreviousList=element(KeyIndex,HashTable),
	NewList=replaceBucket(Key,Value,PreviousList,[]),
	setelement(KeyIndex,HashTable,NewList).


% Adds specified list of key/value pair into the specified hash table.
% If there is already a pair with this key, then its previous value
% will be replaced by the specified one.
addEntries([],HashTable) ->
	HashTable;
	
addEntries( [{EntryName,EntryValue}|Rest], HashTable) ->
	addEntries( Rest, addEntry(EntryName,EntryValue,HashTable) ).
	
	
% Removes specified key/value pair from the specified hash table.
removeEntry(Key,HashTable) ->
	KeyIndex=erlang:phash2(Key,size(HashTable))+1,
	PreviousList=element(KeyIndex,HashTable),
	NewList=deleteBucket(Key,PreviousList,[]),
	setelement(KeyIndex,HashTable,NewList).
	
	
% Looks-up specified entry (designated by its key) in specified hash table.	
% Returns either the atom undefined (if no such key is registered in the table)
% or {value,Value}, with Value being the value associated to the specified key.
lookupEntry(Key,HashTable) ->	
	lookupInList(Key, element(erlang:phash2(Key,size(HashTable))+1,
		HashTable)).


% Tells whether the specified key exists in the table: returns true or false.
hasEntry(Key,HashTable) ->	
	case lookupInList(Key,
			element(erlang:phash2(Key,size(HashTable))+1,HashTable)) of
	
		{value,_} ->
			true; 
	
		undefined ->
			false 
	
	end.	


% Retrieves the value corresponding to specified key and returns it directly.
% The key/value pair is expected to exist already, otherwise a bad match is
% triggered.
getEntry(Key,HashTable) ->	
	{value,Value} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	Value.


% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
% A case clause is triggered if the key did not exist, a bad arithm is
% triggered if no addition can be performed on the associated value.
addToEntry(Key,Value,HashTable) ->	
	{value,Number} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	addEntry(Key,Number+Value,HashTable).


% Substracts specified value to the value, supposed to be numerical, 
% associated to specified key.
% A case clause is triggered if the key did not exist, a bad arithm is
% triggered if no substraction can be performed on the associated value.
substractFromEntry(Key,Value,HashTable) ->	
	{value,Number} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	addEntry(Key,Number-Value,HashTable).


% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
% A case clause is triggered if the entry does not exist or it is not a boolean
% value.
toggleEntry(Key,HashTable) ->	
	case lookupInList(Key,
			element(erlang:phash2(Key,size(HashTable))+1,HashTable)) of
	
		{value,true} ->
			addEntry(Key,false,HashTable); 
	
		{value,false} ->
			addEntry(Key,true,HashTable) 
	
	end.	


% Appends specified element to the value, supposed to be a list, associated to
% specified key.
% A case clause is triggered if the entry does not exist.
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
appendToEntry(Key,Element,HashTable) ->	
	{value,List} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	addEntry(Key,[Element|List],HashTable).
	

% Appends specified element to the value, supposed to be a list, associated to
% specified key.
% A case clause is triggered if the entry does not exist.
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
% Deletes the first match of specified element from the value specified from
% key, that value being supposed to be a list.
% A case clause is triggered if the entry did not exist.
% If the element is not in the specified list, the list will not be modified.
deleteFromEntry(Key,Element,HashTable) ->	
	{value,List} = lookupInList(Key,
		element(erlang:phash2(Key,size(HashTable))+1,HashTable)),
	addEntry(Key,lists:delete(Element,List),HashTable).


% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable. 
popFromEntry(Key,HashTable) ->
	{value,[H|T]} = lookupEntry(Key,HashTable),
	{H,addEntry(Key,T,HashTable)}.

	


% Returns a flat list whose elements are all the key/value pairs of the
% hashtable.
enumerate(Hashtable) ->
	lists:flatten(tuple_to_list(Hashtable)).


% Returns the number of entries (key/value pairs) stored in specified
% hashtable.
getEntryCount(Hashtable) ->
	erlang:length( enumerate(Hashtable) ).
	

% Returns a new hashtable, which started from HashTableBase and was enriched
% with the HashTableAdd entries whose keys where not already in HashTableBase
% (if a key is in both tables, the one from HashTableBase will be kept).
merge(HashTableBase,HashTableAdd) ->
	% Uses the fact that when two entries with the same key are added,
	% the final associated value is the one of the latest to be added.
	lists:foldl(
		fun({Key,Value},Acc) -> addEntry(Key,Value,Acc) end,
		HashTableAdd,
		enumerate(HashTableBase)). 
	

toString(HashTable) when size(HashTable) > 0 ->
	lists:foldl(
		fun(Bucket,Acc) -> 
			Acc ++ io_lib:format(
				"  + ~s~n",[bucket_toString(Bucket)])
		end,
		io_lib:format( "Hashtable with ~B bucket(s) and ~B entry(ies) : ~n",
			[ size(HashTable), hashtable:getEntryCount(HashTable) ]), 
		tuple_to_list(HashTable));

toString(_) ->
	io_lib:format("Empty hashtable~n",[]).




% Displays in the standard output 
display(HashTable) ->
	io:format( "~s",[ toString(HashTable) ]).




% Helper functions.


% Returns a new tuple, whose size is length and whose elements are all set to
% specified default value (Default).
createTuple(Length,Default) ->
	createTuple(Length,Default,[]).

% Final step :	
createTuple(0,_,Accumulator) ->
	list_to_tuple(Accumulator);

% Building from n-1 to n elements :
createTuple(N,Default,Accumulator) ->
	createTuple(N-1,Default,[Default|Accumulator]).
	
	

% Removes pair entry from list when the key matches the specified one : 
% (returns an identical list if the key is not found)		
deleteBucket(Key,[{Key,_}|T],Accumulator) -> 
	% Skips the key if matching:
	lists:append(T,Accumulator);

deleteBucket(Key,[H|T],Accumulator) ->
	% Keeps everything else: 
	deleteBucket(Key,T,[H|Accumulator]);

deleteBucket(_,[],Accumulator) -> 
	Accumulator.

	
% Replaces in specified list a key/value pair by another :	
replaceBucket(Key,Value,[],Accumulator)	->
	[{Key,Value}|Accumulator];

replaceBucket(Key,Value,[{Key,_}|T],Accumulator) ->
	[{Key,Value}|lists:append(T,Accumulator)];

replaceBucket(Key,Value,[H|T],Accumulator) ->
	replaceBucket(Key,Value,T,[H|Accumulator]).
	

		

% Returns a string describing a hashtable bucket (list of key/value pairs) :	
bucket_toString(Bucket) when length(Bucket) > 0 ->
	lists:foldl(
		fun({Key,Value},Acc) ->
			Acc ++ io_lib:format( "     * ~s -> ~s~n", 
				[ utils:term_toString(Key),	utils:term_toString(Value) ])
		end,
		io_lib:format( "Bucket with ~B element(s) :~n",
			[length(Bucket)]),
		Bucket);

bucket_toString(_) ->
	"Empty bucket".

	
% Returns the value corresponding to the key in the specified list : 	
lookupInList(_,[]) ->
	undefined;
	
lookupInList(Key,[{Key,Value}|_]) ->
	{value,Value};

lookupInList(Key,[_|T]) ->
	lookupInList(Key,T).	
	