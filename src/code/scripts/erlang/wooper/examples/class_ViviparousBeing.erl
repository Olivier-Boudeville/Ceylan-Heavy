-module(class_ViviparousBeing).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[]).

-define(wooper_export,getMeanChildrenCount/0).

% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").

% Let's say an average means something here :
getMeanChildrenCount() ->
	4.
	
	
