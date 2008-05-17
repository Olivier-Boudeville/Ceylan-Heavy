% Management of random number generation.
% The random manager is often a singleton.
% Its process is registered under the name ?random_manager_name.
% Centralizing the random generation may be a means of having reproducible
% random values. This is also why this manager is an actor as well.
% See class_RandomManager_test.erl
% Inspired from http://www.trapexit.org/Random_Numbers_Biased 
-module(class_RandomManager).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Mu,Sigma,IsPrivate).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/3,new_link/3,
	synchronous_new/3,synchronous_new_link/3,construct/4,delete/1).


% Method declarations.
% For each get* method, there is get*/n, the direct request, and get*/n+1,
% the counterpart actor oneway, and get*/n+2, the same oneway apart that it 
% returns a series of values (a list) instead of a unique one.
-define(wooper_method_export,
	act/1,
	getUniformValue/2,getUniformValue/3,getUniformValues/3,
	getExponentialValue/2,getExponentialValue/3,getExponentialValues/3,
	getPositiveIntegerExponentialValue/2, getPositiveIntegerExponentialValue/3,
	getPositiveIntegerExponentialValues/3,
	getGaussianValue/1,getGaussianValue/3,getGaussianValues/3,
	getPositiveIntegerGaussianValue/1,
	getPositiveIntegerGaussianValue/3,getPositiveIntegerGaussianValues/3,
	display/1,toString/1).

% Static method declarations (to be directly called from module):
-export([create/0,getManager/0,remove/0]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"RandomManagement").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For random constants:
-include("class_RandomManager.hrl").


% Use global:registered_names() to check manager presence.


% Implementation notes:
%
% There are three classes of built-in random distributions:
%  - uniform (a.k.a. white noise)
%  - exponential 
%  - gaussian (a.k.a. normal)
%
% The random manager is itself an actor, thus it is synchronized by the 
% TimeManager and it uses actor messages (if used as an actor; it can in 
% parallel answer to classical non-synchronized requests as well).
% 
% For each distribution law, following variations are available:
%  - a simple non-synchronized method (ex: getUniformValue/2), returning one
% value to the caller PID (not necessarily an actor)
%  - a simple synchronized method (ex: getUniformValue/3), returning one
% value to the caller actor
%  - a more complex synchronized method (ex: getUniformValues/3) [note the
% final 's'), returning a series of values and an identifier to the caller actor
% so that the caller is able to track multiple distributions simulatenously
%
% The last two cases use actor oneways, they send their answer through a 
% oneway actor call to the caller actor, calling corresponding set*Value/
% set*Values actor method (ex: setUniformValue).
%
% The uniform law can be based either on the random module (random:uniform/1)
% or on the crypto module (crypto:rand_uniform/2). The two forms yield different
% but quite similar results; currently the crypto module is used.
% 
% Exponential and gaussian laws generate by default floating-point numbers.
% For more convenience, counterparts returning positive integer values have 
% been defined (ex: getGaussianValue/getPositiveIntegerGaussianValue).
%
% Note that the random manager is not a StochasticActor: instead it is used by 
% StochasticActor instances so that they can manage their distribution law.



% Constructs a new random manager.
%  - Mu is the mean
%  - Sigma is the standard deviation
%  - IsPrivate tells whether this random manager will be privately held (hence
% should not be registered in naming service) or if it is a (registered) 
% singleton
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, "RandomManager" ),

	% Then the class-specific actions:
	StartingState = ?setAttribute( ActorState, trace_categorization,
		?TraceEmitterCategorization ),
	
	% For crypto:rand_uniform:
	crypto:start(),
	
	PrivateState = case IsPrivate of 
	
		true ->
			?send_trace([ StartingState, io_lib:format( 
				"Creating a private random manager, offering by default "
				"various distributions including a gaussian one, "
				"with mean mu = ~w and variance sigma = ~w.", 
				[ Mu, Sigma ] ) ]),
			?setAttribute(StartingState,is_private,true);
		
		false ->
			?send_trace([ StartingState, io_lib:format( 
				"Creating the global random manager, offering by default "
				"various distributions including a gaussian one, "
				"with mean mu = ~w and variance sigma = ~w.", 
				[ Mu, Sigma ] ) ]),
	
			case global:register_name( ?random_manager_name, self() ) of 
	
				yes ->
					?send_debug([ StartingState,
						"Random manager registered globally." ]);
		
				no ->
					?send_error([ StartingState, 
						"Random manager could not be registered." ]),
					exit( random_manager_could_not_register )
			
			end,

			% Registers as well locally:
			true = register( ?random_manager_name, self() ),
			?setAttribute(StartingState,is_private,false)

	end,
	
	% Needing floating-point values:
	EndState = ?setAttributes( PrivateState, [ { mu, 1.0*Mu }, 
		{ sigma, 1.0*Sigma } ] ),
		
	?send_debug([ EndState, "Random manager created." ]),
	EndState.

	
	
% Overriden destructor.
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting random manager." ]),

	crypto:stop(),
	
	case ?getAttr(is_private) of
	
		true ->
			ok;
			
		false ->	 
			unregister( ?random_manager_name ),
			global:unregister_name( ?random_manager_name )
			
	end,
	?debug([ "Random manager deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).
	
	
	

% Management section of the actor.


% A Random manager is purely passive, so it answers only to incoming actor
% requests and never send messages spontaneously.
act(State) ->

	?trace([ "Random manager acting (and doing nothing)." ]),

	% Random list requests may have been answered this tick:
	?wooper_return_state_only( class_Actor:manage_end_of_tick(State) ).
	
	
	
% Methods section.

% For each random distribution, there is at least a request-based version and
% two oneway versions using actor messages: a simple one returning one random
% value, and a more complex one using an identifier and returning a list of
% random values, instead of only one. The two oneways have the same arity.




%
% Uniform section.
%


% Returns an integer random value generated from an uniform distribution. 
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
% (request method)
getUniformValue(State,N) ->
	%Value = random:uniform(N),
	Value = crypto:rand_uniform(1,N+1),
	%?debug([ io_lib:format( "Returning uniform value ~w.", 
	%	[ Value ] ) ]),	
	?wooper_return_state_result( State,	{uniform_value,Value} ).


% Returns an integer random value generated from an uniform distribution. 
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
% (actor oneway)
getUniformValue(State,N,CallingActorPid) ->
	Value = random:uniform(N),
	%?debug([ io_lib:format( "Returning uniform value ~w to actor ~w.", 
	%	[ Value, CallingActorPid ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setUniformValue,Value}, State ),
	?wooper_return_state_only( NewState ).


% Returns a list of Count integer uniform values in [1,N] (both included) 
% to the calling stochastic actor (CallingActorPid).
% Given an integer N >= 1, returns random integers uniformly distributed
% between 1 and N, updating the random state in the process dictionary.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getUniformValues(State,[N,Count,RequestIdentifier],CallingActorPid) ->
	Values = generate_uniform_list(N,Count),
	%?debug([ io_lib:format( 
	%	"Returning uniform values ~w to actor ~w with identifier ~w.", 
	%	[ Values, CallingActorPid, RequestIdentifier ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setUniformValues,[RequestIdentifier,Values]}, State ),
	?wooper_return_state_only( NewState ).




%
% Exponential section.
%
% Note: each of the three forms comes in two versions, with floating-point 
% or (positive) integer values being returned.
%


% Returns an exponential floating-point random value with Lambda being the
% rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% (request method)
getExponentialValue(State,Lambda) ->
	Value = get_exponential_value(Lambda),
	%?debug([ io_lib:format( "Returning exponential value ~w.", [ Value ] ) ]),	
	?wooper_return_state_result( State,	{exponential_value,Value} ).


% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% (request method)
getPositiveIntegerExponentialValue(State,Lambda) ->
	Value = round( get_exponential_value(Lambda) ),
	%?debug([ io_lib:format( "Returning positive integer exponential value ~w.",
	%	[ Value ] ) ]),	
	?wooper_return_state_result( State,
		{positive_integer_exponential_value,Value} ).



% Returns an exponential floating-point random value with Lambda being the
% rate parameter to the calling actor: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% (actor oneway)
getExponentialValue(State,Lambda,CallingActorPid) ->
	Value = get_exponential_value(Lambda),
	%?debug([ io_lib:format( "Returning exponential value ~w to actor ~w.", 
	%	[ Value, CallingActorPid ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setExponentialValue,Value}, State ),
	?wooper_return_state_only( NewState ).


% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter to the calling actor: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% (actor oneway)
getPositiveIntegerExponentialValue(State,Lambda,CallingActorPid) ->
	Value = round( get_exponential_value(Lambda) ),
	%?debug([ io_lib:format( 
	%	"Returning positive integer exponential value ~w to actor ~w.", 
	%	[ Value, CallingActorPid ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setPositiveIntegerExponentialValue,Value}, State ),
	?wooper_return_state_only( NewState ).



% Returns a list of Count exponential values according to the specified
% Lambda setting to the calling (stochastic) actor (CallingActorPid).
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getExponentialValues(State,[Lambda,Count,RequestIdentifier],CallingActorPid) ->
	Values = generate_exponential_list(Lambda,Count),
	%?debug([ io_lib:format( 
	%	"Returning exponential values ~w to actor ~w with identifier ~w.", 
	%	[ Values, CallingActorPid, RequestIdentifier ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setExponentialValues,[RequestIdentifier,Values]}, State ),
	?wooper_return_state_only( NewState ).


% Returns a list of Count exponential values according to the specified
% Lambda setting to the calling (stochastic) actor (CallingActorPid).
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
% Mean value of drawn samples is 1/Lambda.
% @see http://en.wikipedia.org/wiki/Exponential_distribution
% Using inverse transform sampling.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getPositiveIntegerExponentialValues(State,[Lambda,Count,RequestIdentifier],
		CallingActorPid) ->
	Values = generate_positive_integer_exponential_list(Lambda,Count),
	%?debug([ io_lib:format( "Returning exponential positive integer values ~w "
	%	"to actor ~w with identifier ~w.", 
	%	[ Values, CallingActorPid, RequestIdentifier ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setPositiveIntegerExponentialValues,[RequestIdentifier,Values]}, 
		State ),
	?wooper_return_state_only( NewState ).



%
% Gaussian section.
%
% Note: each of the three forms comes in two version, with floating-point 
% or positive integer values being returned.
%
% Note also that the function order matters, and some share some arities.
%
% Another point of variation is the use of default settings for the gaussian
% laws, instead of a specified one.
%


% Returns a random value generated from the normal (gaussian) distribution
% with default settings. 
% (request method)
getGaussianValue(State) ->
	Value = sigma_loop( ?getAttr(mu), ?getAttr(sigma) ),
	%?debug([ io_lib:format( "Returning default gaussian value ~w.", 
	%	[ Value ] ) ]),	
	?wooper_return_state_result( State,	{gaussian_value,Value} ).
		
% getPositiveIntegerGaussianValue/3 must be declared later 
% (order due to arities).
	
	
% Returns a random value generated from the normal (gaussian) distribution
% with default settings. 
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative. 
% (request method)
getPositiveIntegerGaussianValue(State) ->
	Value = sigma_loop_positive_integer( ?getAttr(mu), ?getAttr(sigma) ),
	%?debug([ io_lib:format( 
	%	"Returning default positive integer gaussian value ~w.", [ Value ] ) ]),
	?wooper_return_state_result( State,
		{positive_integer_gaussian_value,Value} ).



% Reordering was needed, as there are two getGaussianValue/3 
% (one request/one actor oneway).


% Returns a gaussian value with specified settings to the calling stochastic
% actor (CallingActorPid).
% Given a mean Mu and a variance Sigma, returns a random floating-point value
% drawn according to the corresponding gaussian law, updating the state in the
% process dictionary.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getGaussianValue(State,[Mu,Sigma],CallingActorPid) ->
	Value = sigma_loop( Mu, Sigma ),
	%?debug([ io_lib:format( "Returning gaussian value ~w to actor ~w.", 
	%	[ Value, CallingActorPid ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setGaussianValue,Value}, State ),
	?wooper_return_state_only( NewState );
	
% Returns a random value generated from the normal (gaussian) distribution
% with specified settings. 
% (request)
getGaussianValue(State,Mu,Sigma) ->
	Value = sigma_loop( Mu, Sigma ),
	%?debug([ io_lib:format( "Returning gaussian value ~w.", 
	%	[ Value ] ) ]),	
	?wooper_return_state_result( State,	{gaussian_value,Value} ).
	
	

% Returns a list of Count gaussian values to the calling stochastic actor
% (CallingActorPid).
% Given a mean Mu and a variance Sigma, returns random integers drawn according
% the corresponding gaussian law, updating the state in the process dictionary.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getPositiveIntegerGaussianValue(State,[Mu,Sigma],CallingActorPid) ->
	Value = sigma_loop_positive_integer(Mu,Sigma),
	%?debug([ io_lib:format( 
	%	"Returning positive integer gaussian value ~w to actor ~w.", 
	%	[ Value, CallingActorPid ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setPositiveIntegerGaussianValue,Value}, State ),
	?wooper_return_state_only( NewState );

% Returns a random value generated from the normal (gaussian) distribution
% with specified settings. 
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative. 
% (request)
getPositiveIntegerGaussianValue(State,Mu,Sigma) ->
	Value = sigma_loop_positive_integer( Mu, Sigma ),
	%?debug([ io_lib:format( "Returning positive integer gaussian value ~w.", 
	%	[ Value ] ) ]),	
	?wooper_return_state_result( State, 
		{positive_integer_gaussian_value,Value} ).
		





% Returns a list of Count gaussian values to the calling stochastic actor
% (CallingActorPid).
% Given a mean Mu and a variance Sigma, returns random floating-point values
% drawn according the corresponding gaussian law, updating the state in the
% process dictionary.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getGaussianValues(State,[Mu,Sigma,Count,RequestIdentifier],CallingActorPid) ->
	Values = generate_gaussian_list(Mu,Sigma,Count),
	%?debug([ io_lib:format( "Returning gaussian values ~w "
	%	"to actor ~w with identifier ~w.", 
	%	[ Values, CallingActorPid, RequestIdentifier ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setGaussianValues,[RequestIdentifier,Values]}, State ),
	?wooper_return_state_only( NewState ).
	

% Returns a list of Count gaussian values to the calling stochastic actor
% (CallingActorPid).
% Given a mean Mu and a variance Sigma, returns random integers drawn according
% the corresponding gaussian law, updating the state in the process dictionary.
% RequestIdentifier is an identifier (ex: an attribute name) that allows a given
% actor to request more than one random list of a given kind in parallel.
% (actor oneway)
getPositiveIntegerGaussianValues(State,[Mu,Sigma,Count,RequestIdentifier],
		CallingActorPid) ->
	Values = generate_positive_integer_gaussian_list(Mu,Sigma,Count),
	%?debug([ io_lib:format( "Returning  positive integer gaussian values ~w "
	%	"to actor ~w with identifier ~w.", 
	%	[ Values, CallingActorPid, RequestIdentifier ] ) ]),	
	NewState = class_Actor:send_actor_message( CallingActorPid,
		{setPositiveIntegerGaussianValues,[RequestIdentifier,Values]}, State ),
	?wooper_return_state_only( NewState ).


% Other gaussian counterparts with implied {Mu,Sigma} deemed not useful.





% Generic interface.	
	
% Displays the state in the console.
display(State) ->
	wooper_display_instance(State),
	?wooper_return_state_only( State ).


% Returns a textual description of this manager.
toString(State) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).




% 'Static' methods (module functions):
	
	
% Creates the random manager asynchronously, with default settings (mean of 
% zero, sigma of 1).
% (static)	
create() ->
	% mu, sigma, isPrivate:
	new_link(0,1,false).



% Returns the Pid of the current random manager if it exists, otherwise 
% random_manager_not_found. 
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
% (static)
getManager() ->

	% Waits gracefully for the random manager to exist:
	case utils:wait_for_global_registration_of( ?random_manager_name ) of
	
		{registration_waiting_timeout,?random_manager_name} ->
			random_manager_not_found;
			
		RandomManagerPid when is_pid(RandomManagerPid) ->
			RandomManagerPid	
			
	end.
	


% Deletes the random manager.
% (static)	
remove() ->

	case global:whereis_name( ?random_manager_name ) of
	
		undefined ->
			random_manager_not_found;
			
		RandomManagerPid ->
			RandomManagerPid ! delete
			
	end.



% Section for helper functions (not methods).


% generate_*_list could use higher-order functions.


% Generates a list of Count uniform random values between 1 and N.
generate_uniform_list(N,Count) ->
	generate_uniform_list(N,Count,[]).
	
		
generate_uniform_list(_,0,Acc) ->	
	Acc;

generate_uniform_list(N,Count,Acc) ->	
	generate_uniform_list(N,Count-1,
		[ erlang:round( random:uniform(N) ) | Acc ] ).
	
	
	
	
	
% Generates a list of Count exponential random values.
generate_exponential_list(Lambda,Count) ->
	generate_exponential_list(Lambda,Count,[]).
	
		
generate_exponential_list(_,0,Acc) ->	
	Acc;

generate_exponential_list(Lambda,Count,Acc) ->	
	generate_exponential_list(Lambda,Count-1,
		[ get_exponential_value( Lambda )  | Acc ] ).



% Generates a list of Count positive integer exponential random values.
generate_positive_integer_exponential_list(Lambda,Count) ->
	generate_positive_integer_exponential_list(Lambda,Count,[]).
	
		
generate_positive_integer_exponential_list(_,0,Acc) ->	
	Acc;

generate_positive_integer_exponential_list(Lambda,Count,Acc) ->	
	generate_positive_integer_exponential_list(Lambda,Count-1,
		[ erlang:round( get_exponential_value( Lambda ) ) | Acc ] ).





% Generates a list of Count gaussian random values.
generate_gaussian_list(Mu,Sigma,Count) ->
	generate_gaussian_list(Mu,Sigma,Count,[]).
	
		
generate_gaussian_list(_,_,0,Acc) ->	
	Acc;

generate_gaussian_list(Mu,Sigma,Count,Acc) ->	
	generate_gaussian_list(Mu,Sigma,Count-1,
		[ sigma_loop( Mu, Sigma )  | Acc ] ).



% Generates a list of Count positive integer gaussian random values.
generate_positive_integer_gaussian_list(Mu,Sigma,Count) ->
	generate_positive_integer_gaussian_list(Mu,Sigma,Count,[]).
	
		
generate_positive_integer_gaussian_list(_,_,0,Acc) ->	
	Acc;

generate_positive_integer_gaussian_list(Mu,Sigma,Count,Acc) ->	
	generate_positive_integer_gaussian_list(Mu,Sigma,Count-1,
		[ erlang:round( sigma_loop( Mu, Sigma ) ) | Acc ] ).





% Note: with Erlang, math:log(x) is ln(x):
get_exponential_value(Lambda) ->
	-math:log( random:uniform() ) / Lambda.


% Generates a new normal value and updates the state.
% Returns the computed value. 
sigma_loop(Mu,Sigma) ->

    V1 = 2.0 * random:uniform() - 1.0,
    V2 = 2.0 * random:uniform() - 1.0,
    S  = (V1 * V1) + (V2 * V2),
	
	% Loop until S < 1.0:
    if
	
        S >= 1.0 ->
            sigma_loop(Mu,Sigma);
			
        true ->
			% Here S < 1.0:
            Scale = math:sqrt( (-2.0 * math:log(S)) / S ),
			%io:format( "sigma_loop returning a value.~n" ),
			Mu + ( Sigma * Scale * V1)
			
    end.
	
	
% Generates a new integer non-negative normal value and updates the state.
% Returns the computed value. 
sigma_loop_positive_integer(Mu,Sigma) ->
	case round( sigma_loop(Mu,Sigma) ) of
	
		TriedValue when TriedValue<0 ->
			sigma_loop_positive_integer(Mu,Sigma);
		
		NonNegativeValue ->
			NonNegativeValue
		
	end.
	
