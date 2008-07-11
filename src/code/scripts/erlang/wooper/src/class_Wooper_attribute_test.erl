-module(class_Wooper_attribute_test).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).

% Parameters taken by the constructor ('construct'). 
% They are here the ones of the Mammal mother class (the ovoviviparous being 
% constructor does not need any parameter) plus nozzle color.
% These are class-specific data needing to be set in the constructor:
-define(wooper_construct_parameters,).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/0,new_link/0,
	synchronous_new/0,synchronous_new_link/0,construct/1).

% Method declarations.
-define(wooper_method_export,test/1).

% Static method declarations.
-export([run/0]).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-define(Prefix,"--> ").


% Constructs a new test instance.
construct(State) ->
	% Class-specific attributes:
	?setAttribute(State,test_attribute,true).


% Oneway test.
test(State) ->
	io:format( "   Testing attribute management.~n" ),
	
	true        = ?hasAttribute(State,test_attribute),
	false       = ?hasAttribute(State,non_existing),
	
	true        = ?getAttr(test_attribute),
	
	UnsetState  = ?removeAttribute(State,test_attribute),
	
	NewSetState = ?setAttribute(UnsetState,test_attribute,true),
	true        = ?getAttribute(NewSetState,test_attribute),
	
	MultiState  = ?setAttributes(NewSetState,[
		{test_attribute,false}, {another_attribute,42} ]),
	false       = ?getAttribute(MultiState,test_attribute),
	42          = ?getAttribute(MultiState,another_attribute),
	
	RevertState = ?toggleAttribute(MultiState,test_attribute),
	true        = ?getAttribute(RevertState,test_attribute),
	
	VoidState   = ?setAttribute(RevertState,test_list,[]),
	AppendState = ?appendToAttribute(VoidState,test_list, 7),
	AgainState  = ?appendToAttribute(AppendState,test_list, 8),
	[8,7]       = ?getAttribute(AgainState,test_list),
	
	DeleteState = ?deleteFromAttribute(AgainState,test_list,7),
	[8]         = ?getAttribute(DeleteState,test_list),
	
	PreAddState = ?setAttribute(DeleteState,test_add,1),
	AddState    = ?addToAttribute(PreAddState,test_add,10),
	11          = ?getAttribute(AddState,test_add),
	
	SubState    = ?substractFromAttribute(AddState,test_add,5),
	6           = ?getAttribute(SubState,test_add),

	not_crashing_examples(SubState),
	crashing_examples(SubState),
	
	io:format( "   End of attribute management test.~n" ),	
	?wooper_return_state_result( SubState, test_ok ).


not_crashing_examples(State) ->
	NewState = ?removeAttribute(State,non_existing),
	OtherNewState = ?appendToAttribute(NewState,test_attribute,8),
	io:format( "List is ~w.~n", 
		[ ?getAttribute(OtherNewState,test_attribute) ]).


crashing_examples(State) ->
	%?toggleAttribute(State,non_existing),
	% Not a boolean:
	%?toggleAttribute(State,test_add),
	
	%?addToAttribute(State,non_existing,4),
	% Not a number:
	%?addToAttribute(State,test_attribute,4),
	
	%?substractFromAttribute(State,non_existing,4),
	% Not a number:
	%?substractFromAttribute(State,test_attribute,4),

	% Not a list:
	%?deleteFromAttribute(State,test_attribute,7),
	
	State,
	%?getAttr(non_existing),
	ok.
	

% Actual test.
% (static)
run() ->
	Tested = class_Wooper_attribute_test:new_link(),
	Tested ! {test,[],self()},
	receive
	
		{wooper_result,test_ok} ->
			io:format( ?Prefix "Test success.~n" ),
			erlang:halt();
			
		Other ->
			io:format( ?Prefix "Test failed: ~w.~n", [Other] ),
			erlang:exit(test_failed)
			
	end.
	

