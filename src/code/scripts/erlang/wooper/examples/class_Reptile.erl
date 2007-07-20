-module(class_Reptile).


% Determines what are the mother classes of this class (if any) :
-define(superclasses,[class_Creature]).

-define(wooper_export,isHotBlooded/0,canMoult/0).

% Allows to define WOOPER base variables and methods for that class :
-include("wooper_class_root.hrl").


% Implement your methods here once listed in wooper_export.
	
isHotBlooded() ->
	false.
	
canMoult() ->
	true.
	
