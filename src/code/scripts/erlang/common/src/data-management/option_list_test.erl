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
% Creation date: Saturday, February 20, 2010.

% Unit tests for the option list implementation.
% See the option_list.erl tested module.

-module(option_list_test).


-export([run/0]).


-define(Tested_module,option_list).


run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	SingleOptionList = [{blue,2}],

	% Pattern-match:
	SingleOptionList = option_list:set( {blue,2}, [] ),

	InitialOptionList = [ {yellow,1}, {blue,1}, {red,1}, {green,1}, 
						  {purple,1} ],

	io:format( "   Initial option_list: ~w.~n", [InitialOptionList] ),

	BlackOptionList = option_list:set( {black,1}, InitialOptionList ),
	io:format( "   OptionList with black entry added: ~w.~n", [BlackOptionList] ),

	RedOptionList = option_list:set( {red,2}, BlackOptionList ),
	io:format( "   OptionList with red entry incremented: ~w.~n", [RedOptionList] ),
	
	EndpointOption_List = option_list:set( {black,2}, 
      	option_list:set( {purple,2}, RedOptionList ) ),				  
	
	io:format( "   OptionList with endpoints updated: ~w.~n", [EndpointOption_List] ),

	SecondOptionList = option_list:set( {magenta,1}, SingleOptionList ),
	UpdatingOptionList = option_list:set( {black,3}, SecondOptionList ),

	io:format( "   Update of previous option_list with option_list ~w is: ~w.~n", 
      [ UpdatingOptionList, option_list:update_with(
                           EndpointOption_List,UpdatingOptionList) ] ),
	
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),	
	erlang:halt().

