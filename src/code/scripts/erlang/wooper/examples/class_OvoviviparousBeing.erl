-module(class_OvoviviparousBeing).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[]).

-define(wooper_export,getMeanEggCount/0).

% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").

% Let's say an average means something here :
getMeanEggCount() ->
	1000.
	
	
