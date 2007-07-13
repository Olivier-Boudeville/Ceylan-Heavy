% Generic hash table implementation.
% See hashtable_test.erl for the corresponding test.

-module(hashtable).

% Heavily inspired of the tupleStore example from 
% 'Concurrent Programming in Erlang', section 9.8.

% The hashtable is implemented thanks to a tuple whose size is the number of
% buckets specified at the hashtable creation.
% Each tuple element (hence each bucket) is a list of key/value pairs.

% Maybe the ets module could/should be used instead.


-export([new/0,new/1,addEntry/3,removeEntry/2,lookupEntry/2,enumerate/1,
	merge/2,toString/1,display/1]).


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


% Returns a flat list whose elements are all the key/value pairs of the
% hashtable.
enumerate(Hashtable) ->
	lists:flatten(tuple_to_list(Hashtable)).
	

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
		io_lib:format( "Hashtable with ~B buckets : ~n",
			[size(HashTable)]), 
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
deleteBucket(Key,[{Key,_}|T],Accumulator) -> 
	lists:append(T,Accumulator);

deleteBucket(Key,[_|T],Accumulator) -> 
	deleteBucket(Key,[T],Accumulator);

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
	
