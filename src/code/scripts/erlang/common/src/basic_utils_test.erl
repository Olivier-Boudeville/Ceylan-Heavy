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


% Unit tests for the basic utils toolbox.
% See the basic_utils.erl tested module.
-module(basic_utils_test).

-export([run/0]).

-define(Tested_module,basic_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	InitialTimestamp = basic_utils:get_timestamp(),
	
	io:format( "   Timestamp is ~s.~n", [ 
		basic_utils:get_textual_timestamp(InitialTimestamp) ] ),
				
	io:format( "   Output with term_toString : ~s, ~s and ~s.~n", 
		[ basic_utils:term_toString(an_atom), basic_utils:term_toString([1,2]), 
			basic_utils:term_toString("A string")	]),
	
	io:format( "   Converting an integer to a string: ~s.~n",
		[ basic_utils:integer_to_string(3245) ] ),		

	basic_utils:checkpoint(1),
	
	basic_utils:start_random_source( default_seed ),
	
	RandomList = [ basic_utils:get_random_value(5) || _X <- lists:seq(1,15) ],

	basic_utils:stop_random_source(),
	
	basic_utils:checkpoint(2),
	
	io:format( "   Current module being used as random source: ~w.~n",
		[basic_utils:get_random_module_name()] ),
			
	io:format( "   A list of integer random values between 1 and 5 "
		"(both included): ~w.~n", [RandomList] ),
	
	TestName = "I have spaces and also 'I have quotes'",
		
	io:format( "   Filename generated from '~s' is '~s'.~n", 
		[TestName,file_utils:convert_to_filename(TestName)] ),
	
	
	% Testing list management:
	L = [ 12, 4, 13, 2, 56, 0 ],
	
	GetIndex = 3,
	
	GetValue = basic_utils:get_element_at(L,GetIndex),
	io:format( "   Getting item #~B of list ~w: ~B.~n", [GetIndex,L,GetValue] ),
	
	13 = GetValue,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   Getting item #~B of list ~w: ~B.~n", [OutOfBoundsIndex,L,
	%	basic_utils:get_element_at(L,OutOfBoundsIndex) ] ),    
	
	RemoveIndex = 3,
	
	ShortenList = basic_utils:remove_element_at( L, RemoveIndex ),
	
	io:format( "   List obtained after having removed item #~B of list ~w: "
		" ~w.~n", [RemoveIndex,L,ShortenList] ),

	% Hardcoded for checking:
	CorrectShortenList = [ 12, 4, 2, 56, 0 ],
	
	ShortenList = CorrectShortenList,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   List obtained after having removed item #~B of list ~w: "
	%	" ~w.~n", [OutOfBoundsIndex,L,
	%	basic_utils:remove_element_at( L, OutOfBoundsIndex )] ),

	io:format( "   List obtained after having uniformly permuted list ~w: "
		" ~w.~n", [L,basic_utils:random_permute(L)] ),

	io:format( "   List obtained after having uniformly permuted list ~w "
		"(again): ~w.~n", [L,basic_utils:random_permute(L)] ),

	L1 = [1,2,3,4,2],
	
	L2 = [2,3],
	
	Subtracted = basic_utils:subtract_all_duplicates( L1, L2 ),  
	
	io:format( "   Displaying the subtraction with duplicates removal "
		"of ~w by ~w: ~w.~n", [ L1, L2, Subtracted ] ),

	[1,4] = Subtracted, 	
	
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

