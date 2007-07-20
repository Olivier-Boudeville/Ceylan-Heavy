-module(class_Platypus).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[class_Mammal,class_OvoviviparousBeing]).

-define(wooper_export,canEat/1).

% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").


canEat(leaf) ->	
	true;
	
canEat(weed) ->	
	true;
	
canEat(fish) ->	
	true;

canEat(_) ->
	false.	
	
