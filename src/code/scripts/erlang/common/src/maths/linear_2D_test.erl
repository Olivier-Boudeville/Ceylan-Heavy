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


% Unit tests for the linear 2D facilities.
% See the  tested module.
-module(linear_2D_test).


-export([ run/0 ]).

-define(Tested_module,linear_2D).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	InitialTimestamp = linear_2D:get_timestamp(),
	
	io:format( "   Timestamp is ~s.~n", [ 
		linear_2D:get_textual_timestamp(InitialTimestamp) ] ),
				
	io:format( "   Output with term_toString : ~s, ~s and ~s.~n", 
		[ linear_2D:term_toString(an_atom), linear_2D:term_toString([1,2]), 
			linear_2D:term_toString("A string")	]),
	
	io:format( "   Converting an integer to a string: ~s.~n",
		[ linear_2D:integer_to_string(3245) ] ),		

	linear_2D:checkpoint(1),
	
	linear_2D:start_random_source( default_seed ),
	
	RandomList = [ linear_2D:get_random_value(5) || _X <- lists:seq(1,15) ],

	linear_2D:stop_random_source(),
	
	linear_2D:checkpoint(2),
	
	io:format( "   Current module being used as random source: ~w.~n",
		[linear_2D:get_random_module_name()] ),
			
	io:format( "   A list of integer random values between 1 and 5 "
		"(both included): ~w.~n", [RandomList] ),
	
	TestName = "I have spaces and also 'I have quotes'",
		
	io:format( "   Filename generated from '~s' is '~s'.~n", 
		[TestName,file_utils:convert_to_filename(TestName)] ),
	
	
	% Testing list management:
	L = [ 12, 4, 13, 2, 56, 0 ],
	
	GetIndex = 3,
	
	GetValue = linear_2D:get_element_at(L,GetIndex),
	io:format( "   Getting item #~B of list ~w: ~B.~n", [GetIndex,L,GetValue] ),
	
	13 = GetValue,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   Getting item #~B of list ~w: ~B.~n", [OutOfBoundsIndex,L,
	%	linear_2D:get_element_at(L,OutOfBoundsIndex) ] ),    
	
	RemoveIndex = 3,
	
	ShortenList = linear_2D:remove_element_at( L, RemoveIndex ),
	
	io:format( "   List obtained after having removed item #~B of list ~w: "
		" ~w.~n", [RemoveIndex,L,ShortenList] ),

	% Hardcoded for checking:
	CorrectShortenList = [ 12, 4, 2, 56, 0 ],
	
	ShortenList = CorrectShortenList,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%io:format( "   List obtained after having removed item #~B of list ~w: "
	%	" ~w.~n", [OutOfBoundsIndex,L,
	%	linear_2D:remove_element_at( L, OutOfBoundsIndex )] ),

	io:format( "   List obtained after having uniformly permuted list ~w: "
		" ~w.~n", [L,linear_2D:random_permute(L)] ),

	io:format( "   List obtained after having uniformly permuted list ~w "
		"(again): ~w.~n", [L,linear_2D:random_permute(L)] ),

	L1 = [1,2,3,4,2],
	
	L2 = [2,3],
	
	Subtracted = linear_2D:subtract_all_duplicates( L1, L2 ),  
	
	io:format( "   Displaying the subtraction with duplicates removal "
		"of ~w by ~w: ~w.~n", [ L1, L2, Subtracted ] ),

	[1,4] = Subtracted, 	
	
	UnregisteredName = test_non_registered,
	try linear_2D:get_registered_pid_for( UnregisteredName ) of
	
		_Anything ->
			throw( test_should_have_failed )
			
	catch
	
		{neither_registered_locally_nor_globally,UnregisteredName} ->
			ok
			
	end,
	
	RegisteredName = test_registered,
	PidToRegister = self(),
	linear_2D:register_as( PidToRegister, RegisteredName, global_only ),
	
	try linear_2D:get_registered_pid_for( RegisteredName ) of
	
		PidToRegister ->
			ok
			
	catch
	
		Exception ->
			throw( {test_should_have_succeeded,Exception} )
			
	end,
	
	
	ListOfStrings = [ "Hello", "World", "Vampire" ],
		
	io:format( "   Displaying list ~p as a string:~n~s~n", 
		[ ListOfStrings, linear_2D:string_list_to_string(ListOfStrings) ] ),	
		
		
	LongLine = "This is a long line to test the paragraph formatting.",
	
	% So that "formatting." has a chance to fit:
	TargetWidth = 10,
	
	io:format( "   Displaying text '~s' once formatted "
		"for a width of ~B:~n~p~n",
		[LongLine,TargetWidth,
			linear_2D:format_text_for_width(LongLine,TargetWidth) ] ),


	JustWideEnoughLine = "<0.33.0>",
	
	% So that "formatting." has a chance to fit:
	NewTargetWidth = 8,
	
	io:format( "   Displaying text '~s' once formatted "
		"for a width of ~B:~n~p~n",
		[JustWideEnoughLine,NewTargetWidth,
			linear_2D:format_text_for_width( JustWideEnoughLine,
				NewTargetWidth) ] ),
	
	
	FirstTestString = "Hello world!",
	
	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ FirstTestString, linear_2D:is_string(FirstTestString) ] ),
			 	
				
	SecondTestString = [ $o, [ $s, $d ], $l ],
	
	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ SecondTestString, linear_2D:is_string(SecondTestString) ] ),
	
			 	
	ThirdTestString = [ $e, 1, 2, $r ],
	
	io:format( "   Determining whether '~p' is a string: ~w.~n",
		[ ThirdTestString, linear_2D:is_string(ThirdTestString) ] ),
			 	
	FirstVersion  = {0,0,0},
	SecondVersion = {0,0,1},
	ThirdVersion  = {0,1,0},
	FourthVersion = {1,0,0},
	FifthVersion  = {1,1,1},
					
	first_bigger = linear_2D:compare_versions( SecondVersion, FirstVersion),
	first_bigger = linear_2D:compare_versions( ThirdVersion, SecondVersion),
	first_bigger = linear_2D:compare_versions( FifthVersion, FirstVersion),

	second_bigger = linear_2D:compare_versions( FirstVersion, FourthVersion),
	second_bigger = linear_2D:compare_versions( ThirdVersion, FourthVersion),
	second_bigger = linear_2D:compare_versions( SecondVersion, ThirdVersion),
	
	equal = linear_2D:compare_versions( FirstVersion, FirstVersion ),
	equal = linear_2D:compare_versions( ThirdVersion, ThirdVersion ),
	equal = linear_2D:compare_versions( FifthVersion, FifthVersion ),
	
	io:format( "   Comparisons of versions like ~s succeeded.~n", 
		[ linear_2D:version_to_string(ThirdVersion) ] ),
		  
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

