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


% Unit tests for the net_utils toolbox.
% See the net_utils.erl tested module.
-module(net_utils_test).


-export([ run/0 ]).


-define(Tested_module,net_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	
	Localhost = net_utils:localhost(),
	
	io:format( "   Pinging now localhost ~s.~n", [ Localhost ] ),
	
	case net_utils:ping( Localhost ) of
	
		true ->
			io:format( "   Ping success of localhost.~n");
			
		false ->	
			throw( could_not_ping_localhost )
			
	end,

		
	case net_utils:ping( "non.existing.host" ) of
	
		true ->
			throw( could_ping_non_existing_localhost );
			
		false ->	
			io:format( 
				"   Ping could not ping a non-existing host, as expected.~n")
			
	end,
	


	FirstIP = {74,125,127,100},
	io:format( "   Reverse look-up of ~p is '~s'.~n", 
		[ basic_utils:ipv4_to_string(FirstIP), 
		  net_utils:reverse_lookup(FirstIP) ] ),


	SecondIP = {82,225,152,215},
	io:format( "   Reverse look-up of ~p is '~s'.~n", 
		[ basic_utils:ipv4_to_string(SecondIP), 
		  net_utils:reverse_lookup(SecondIP) ] ),


	ThirdIP = {90,59,94,64},
	io:format( "   Reverse look-up of ~p is '~s'.~n", 
		[ basic_utils:ipv4_to_string(ThirdIP), 
		  net_utils:reverse_lookup(ThirdIP) ] ),

	FourthIP = {10,22,22,22},
	io:format( "   Reverse look-up of ~p is '~s'.~n", 
		[ basic_utils:ipv4_to_string(FourthIP), 
		  net_utils:reverse_lookup(FourthIP) ] ),


	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

