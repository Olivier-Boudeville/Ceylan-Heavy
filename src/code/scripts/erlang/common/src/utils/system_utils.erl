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
% Creation date: Thursday, February 11, 2010.


% Gathering of various system convenient facilities.
% See system_utils_test.erl for the corresponding test.
-module(system_utils).



% User-related functions.
-export([ get_user_name/0, get_user_home_directory/0 ]).

		 
% System-related functions.
-export([ get_interpreter_version/0, get_size_of_vm_word/0, get_size/1,
		 interpret_byte_size/1, get_memory_summary/0 ]).



% User-related functions.


% Returns the name of the current user.
get_user_name() ->
	os:getenv("USER").
	

% Returns the home directory of the current user.
get_user_home_directory() ->
	os:getenv("HOME").
	
	


% System-related functions.


% Returns the version informations of the current Erlang interpreter 
% being used.
get_interpreter_version() ->
	erlang:system_info(otp_release).


% Returns the size, in bytes, of a word of this Virtual Machine.
get_size_of_vm_word() ->
	erlang:system_info(wordsize).


% Returns the size of specified term, in bytes.
get_size( Term ) ->
	erts_debug:flat_size(Term) * get_size_of_vm_word().


% Returns a string containing a user-friendly description of the specified size
% in bytes, using GiB (Gibibytes, not Gigabytes), MiB (Mebibytes, not
% Megabytes), KiB (Kibibytes, not Kilobytes) and bytes.
%
% See http://en.wikipedia.org/wiki/Kibibyte
interpret_byte_size( SizeInBytes ) ->
	
	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,
	
    ListWithGiga = case SizeInBytes div Giga of
					 
					 0 ->
						 [];
					 
					 GigaNonNull->
						 [io_lib:format( "~B GiB", [GigaNonNull] )]
							   
				   end,
	SizeAfterGiga = SizeInBytes rem Giga,
	%io:format( "SizeAfterGiga = ~B.~n", [SizeAfterGiga] ),
	
	ListWithMega = case SizeAfterGiga div Mega of
					 
					 0 ->
						 ListWithGiga;
					 
					 MegaNonNull->
						 [io_lib:format( "~B MiB", [MegaNonNull] )|ListWithGiga]
							   
				   end,
	SizeAfterMega = SizeAfterGiga rem Mega,
	%io:format( "SizeAfterMega = ~B.~n", [SizeAfterMega] ),
	
	ListWithKilo = case SizeAfterMega div Kilo of
					 
					 0 ->
						 ListWithMega;
					 
					 KiloNonNull->
						 [io_lib:format( "~B KiB", [KiloNonNull] )|ListWithMega]
							   
				   end,
	SizeAfterKilo = SizeAfterMega rem Kilo,
	%io:format( "SizeAfterKilo = ~B.~n", [SizeAfterKilo] ),
	
	ListWithByte = case SizeAfterKilo of
					 
					 0 ->
						ListWithKilo ;
					 
					 1->
						 [ "1 byte" | ListWithKilo ];
					   
					 AtLeastTwoBytes ->
						 [ io_lib:format( "~B bytes", [AtLeastTwoBytes] )
						   | ListWithKilo ]
							   
				   end,
													
	%io:format( "Unit list is: ~w.~n", [ListWithByte] ),
	
	% Preparing for final display:
	case ListWithByte of
		
		[] ->
			"0 byte";
		
		[OneElement] ->
			OneElement;
		
		[Smaller|Bigger] ->
			text_utils:join( ", ", lists:reverse(Bigger) ) ++ " and " ++ Smaller
	
	end.

	
	

% Returns a summary of the dynamically allocated memory currently being
% used by the Erlang emulator.
get_memory_summary() ->
	SysSize  = erlang:memory( system ),
	ProcSize = erlang:memory( processes ),
	Sum = SysSize + ProcSize,
	io:format( "  - system size: ~s (~s)~n", 
			  [ interpret_byte_size(SysSize), 
			   text_utils:percent_to_string(SysSize/Sum) ] ),
	io:format( "  - process size: ~s (~s)~n", 
			  [ interpret_byte_size(ProcSize), 
			   text_utils:percent_to_string(ProcSize/Sum) ] ).
	
	
	
