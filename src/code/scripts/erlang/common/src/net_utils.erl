% 
% Copyright (C) 2003-2009 Olivier Boudeville
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
-export([ ping/1, localhost/0 ]).




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



% Returns an appropriate name for the local host.
localhost() ->
	% Depending on the node being launched with either:
	%  - no network name or a short name
	%  - a long name
	% net_adm:localhost() may return respectively "XXX.domain.com" or
	% "XXX.localdomain", both of which are not proper hostnames.
	% Most reliable (ending carriage return must be removed):
	basic_utils:remove_ending_carriage_return( os:cmd( "hostname -f" ) ).
	
