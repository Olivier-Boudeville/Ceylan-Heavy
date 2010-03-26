% Copyright (C) 2003-2010 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
% See net_utils_test.erl for the corresponding test.
-module(net_utils).




% Hostname-related functions.
-export([ ping/1, reverse_lookup/1, localhost/0 ]). 


% Node-related functions.
-export([ localnode/0, get_all_connected_nodes/0, 
		 check_node_availability/1, check_node_availability/2, 
		 shutdown_node/1 ]).


% Address-related functions.
-export([ ipv4_to_string/1, ipv4_to_string/2 ]).




% Hostname-related functions.


% Pings specified hostname, and returns true iff it could be ping'd. 
% Note: command-line based call, used that way as there is no ICMP stack.
% A port could be used also.
ping(Hostname) when is_list(Hostname) ->
	Command = "if ping " ++ Hostname ++ " -q -c 1 1>/dev/null 2>&1; "
		"then echo ping_ok ; else echo ping_failed ; fi",
	%io:format( "Ping command: ~s~n.", [Command] ),	
	case os:cmd( Command ) of 
	
		"ping_ok\n" ->
			true ;
		
		"ping_failed\n" ->
			false
				
	end.			 	



% Returns an appropriate DNS name for the local host, or throws an exception.
localhost() ->
	% Depending on the node being launched with either:
	%  - no network name or a short name
	%  - a long name
	% net_adm:localhost() may return respectively "XXX.domain.com" or
	% "XXX.localdomain", both of which are not proper hostnames.
	% On the other hand, "hostname -f" might return 'localhost.localdomain'.
	% Most reliable (ending carriage return must be removed):
	case text_utils:remove_ending_carriage_return( os:cmd( "hostname -f" ) ) of
	
		"localhost" ->
			throw( could_not_determine_localhost );
			
		"localhost.localdomain" ->
			throw( could_not_determine_localhost );
			
		Other ->
			Other
	end.
	

	
% Returns a string specifying the DNS name corresponding to the specified
% IP address {N1,N2,N3,N4}.	
reverse_lookup( IPAddress ) ->
	Command = "host -W 1 " ++ ipv4_to_string(IPAddress) ++ " 2>/dev/null",
	Res = os:cmd( Command ),
	%io:format( "Host command: ~s, result: ~s.~n", [Command,Res] ),	
	case string:tokens( Res," " ) of 
	
		[ _ArpaString, "domain", "name", "pointer", Domain] ->
			% Removes ending ".~n":
			string:sub_string( Domain, 1, length(Domain)-2 );
		
		_Other  ->
			unknown_dns
				
	end.	



	
% Node-related functions.


% Returns the name of the local node, as an atom.
% It is either a specific node name, or the atom 'local_node' (preferred 
% to 'nonode@nohost').
localnode() ->
	case node() of
	
		nonode@nohost ->
			local_node;
			
		OtherNodeName ->
			% Could be XX@myhost.example.com:
		 	OtherNodeName
	
	end.



% Returns the list of all connected nodes (each being designated by an atom,
% like 'foo@bar.org'), including the local node.
get_all_connected_nodes() ->
	[node()|nodes()].



% Returns whether specified Erlang node is available, waiting a bit (up to
% 3.1 seconds) should the node.
% Nodename can be an atom or a string.
% Performs a fixed number of attempts with some exponential waiting in-between, 
% in case the node is being launched in the background.
% Durations are in milliseconds, maximum waiting time is 3.1 seconds. 
% Allows to return as soon as possible. 
check_node_availability( Nodename ) ->
	check_node_availability( Nodename, with_waiting ).
	

	
% Returns whether specified Erlang node is available:
%   - Nodename is an atom or a string corresponding to the name of the target
% node
%   - Timing is either 'immediate' or 'with_waiting'
% If 'immediate', the target node will be deemed available or not as soon as the
% first and only ping attempted returns a result.
% If ' with_waiting', a fixed number of attempts with some exponential waiting
% in-between will be performed. This is useful, if the node is being launched
% in the background, to wait for it, but to return as soon as possible. 
% Durations are in milliseconds, maximum waiting time is 3.1 seconds. 
check_node_availability( Nodename, Timing ) when is_list(Nodename) ->
	check_node_availability( list_to_atom(Nodename), Timing ) ;

check_node_availability( Nodename, _Timing = immediate ) 
		when is_atom(Nodename) ->
	
	case net_adm:ping( Nodename ) of
	
		pong ->
			true ;
		
		pang ->
			false
			
	end;
	
check_node_availability( Nodename, _Timing = with_waiting ) 
		when is_atom(Nodename) ->
	check_node_availability( Nodename, _AttemptCount = 5, 
		_InitialDuration = 100 ).
	

check_node_availability( _AtomNodename, _Count = 0, _CurrentDuration ) ->
	false ;
		
check_node_availability( AtomNodename, AttemptCount, CurrentDuration ) ->

	case net_adm:ping( AtomNodename ) of
	
		pong ->
			true ;
		
		pang ->
			timer:sleep(CurrentDuration),
			check_node_availability( AtomNodename, AttemptCount-1,
				2*CurrentDuration )
			
	end.			
			


% Shutdowns specified node, and returns only when it cannot be ping'ed 
% anymore.
% Throws an exception if not able to terminate it.
shutdown_node(Nodename) when is_list(Nodename) ->
	shutdown_node( list_to_atom(Nodename) );
	
shutdown_node(Nodename)	when is_atom(Nodename) ->
	rpc:cast( Nodename, erlang, halt, [] ),
	wait_unavailable( Nodename, _AttemptCount = 5, _Duration = 100 ).
	
	
wait_unavailable( Nodename, _AttemptCount = 0, _Duration ) ->
	throw( {node_not_terminating,Nodename} );
	
wait_unavailable( Nodename, AttemptCount, Duration ) ->
	case net_adm:ping( Nodename ) of
	
		pong ->
			timer:sleep(Duration),
			wait_unavailable( Nodename, AttemptCount-1, 2*Duration );
			
		pang ->
			ok
			
	end.			
	
	
	
					
% Address-related functions.
	
		
% Returns a string describing the specified IPv4 address.
ipv4_to_string( {N1,N2,N3,N4} ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B", [N1,N2,N3,N4] ) ).
	
	
% Returns a string describing the specified IPv4 address and port.
ipv4_to_string( {N1,N2,N3,N4}, Port ) ->
	lists:flatten( io_lib:format( "~B.~B.~B.~B:~B", [N1,N2,N3,N4,Port] ) ).

