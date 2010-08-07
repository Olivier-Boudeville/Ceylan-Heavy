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


% Unit tests for the file_utils toolbox.
% See the file_utils.erl tested module.
-module(file_utils_test).

-export([run/0]).

-define(Tested_module,file_utils).



run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),

	{ok,CurrentDir} = file:get_cwd(),

	{_RegularFiles,_Directories,_OtherFiles,_Devices} = Elements
		= file_utils:list_dir_elements( CurrentDir ),

	BeamExtension = ".beam",

	io:format( "   File elements in the current directory:~n~p~n", [Elements] ),

	% Too many outputs:
	%io:format( "   Regular BEAM files in the current directory:~n~p~n",
	%	[ file_utils:filter_by_extension(RegularFiles,BeamExtension) ] ),

	io:format( "   All files found recursively "
		"from the current directory:~n~p~n",
		[ file_utils:find_files_from(CurrentDir) ] ),


	io:format( "   All BEAM files found recursively "
		"from the current directory:~n~p~n",
		[ file_utils:find_files_with_extension_from( CurrentDir,
													 BeamExtension ) ] ),

	ExcludedDirs = [ ".svn", "non-existing-dir" ],

	io:format( "   All files found recursively "
		"from the current directory, with directories ~p excluded:~n~p~n",
		[ ExcludedDirs, file_utils:find_files_with_excluded_dirs(
						  CurrentDir, ExcludedDirs ) ] ),

	ErlExtension = ".erl",

	io:format( "   All ~s files found recursively "
		"from the current directory, with directories ~p excluded:~n~p~n",
		[ ErlExtension, ExcludedDirs,
		  file_utils:find_files_with_extension_and_excluded_dirs(
						  CurrentDir, ErlExtension, ExcludedDirs ) ] ),


	FirstFilename = "media/frame/1-23-2-98.oaf",

	io:format( "   Path '~s', once transformed into a variable name, "
		"results in: ~s~n",
		[ FirstFilename, file_utils:path_to_variable_name(FirstFilename) ] ),


	SecondFilename = "./mnt/zadok/44_12.oaf",

	io:format( "   Path '~s', once transformed into a variable name, "
		"results in: ~s~n",
		[ SecondFilename, file_utils:path_to_variable_name(SecondFilename) ] ),


	FirstString = "My name is Bond",
	io:format( "   String '~s', once transformed into a file name, "
		"results in: '~s'~n",
		[ FirstString, file_utils:convert_to_filename(FirstString) ] ),


	SecondString = "James,  James <Bond> ('Special' \"Agent\"), Sir",
	io:format( "   String '~s', once transformed into a file name, "
		"results in: '~s'~n",
		[ SecondString, file_utils:convert_to_filename(SecondString) ] ),


	SourceFilename = "/home/jack/rosie.ttf",
	SourceExtension = ".ttf",
	TargetExtension = ".wav",

	NewFilename = file_utils:replace_extension( SourceFilename, SourceExtension,
		TargetExtension ),

	io:format( "   Replacing extension '~s' by '~s' in '~s' results in: "
		" '~s'.~n",
		[SourceExtension,TargetExtension,SourceFilename,NewFilename] ),

	%file_utils:create_directory( "tmp-tst" ),
	%file_utils:create_directory( "tmp-tst/first/second", create_parents ),

	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().
