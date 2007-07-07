% Unit tests for the generic hash table implementation.
% See the hashtable.erl tested module.

-module(hashtable_test).

-export([run/0]).

-define(Tested_module,hashtable).

run() ->
	io:format( "Testing module ~s.~n", [ ?Tested_module ] ),
	MyH1=hashtable:new(0),
	hashtable:display(MyH1),
	MyH2=hashtable:new(4),
	MyH3=hashtable:addEntry("MyFirstKey","MyFirstValue",MyH2),
	MyH4=hashtable:addEntry("AnotherKey",[1,2,3],MyH3),
	hashtable:display(MyH4),
	io:format( "   Looking up for ~s : ~w~n", [ "MyFirstKey",
		hashtable:lookupEntry("MyFirstKey",MyH4)]),
	io:format( "   Removing that entry.~n" ),
	MyH5=hashtable:removeEntry("MyFirstKey",MyH4),
	io:format( "   Looking up for ~s : ~w~n", [ "MyFirstKey",
		hashtable:lookupEntry("MyFirstKey",MyH5)]),
	hashtable:display(MyH5),
	io:format( "   Testing double key registering.~n" ),
	MyH6=hashtable:addEntry("AnotherKey",anything,MyH5),
	hashtable:display(MyH6),
	io:format( "   Enumerating the hash table : ~w~n",
		[hashtable:enumerate(MyH4)]),
	MyH7=hashtable:addEntry("Third key",3,MyH6),
	% MyH8 should have {AnotherKey,[1,2,3]} and {"Third key",3} :
	MyH8=hashtable:merge(MyH4,MyH7),
	io:format( "   Merged table : ~s~n", [hashtable:toString(MyH8)]),
	io:format( "End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().
