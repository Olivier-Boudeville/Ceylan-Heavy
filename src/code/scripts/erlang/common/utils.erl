% Gathering of various convenient facilities.
% See utils_test.erl for the corresponding test.

-module(utils).

% Creation date: July 1, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL.


-export([get_timestamp/0,get_image_file_gif/1,term_toString/1]).


-define(ResourceDir,"../resources").


% Returns a string corresponding to the current timestamp.
get_timestamp() ->
 	io_lib:format( "[~p:~p:~p]", tuple_to_list(now()) ).
	
	
% Returns the image path corresponding to the specified file.	
get_image_file_gif(Image) ->
  filename:join([?ResourceDir, "images", Image ++ ".gif"]).


% Returns a human-readable string describing specified term.	
term_toString(Term) ->
	case io_lib:printable_list(Term) of
	
		true -> io_lib:format("~s",[Term]);
		
		_    -> io_lib:format("~p",[Term])
		
	end.	
		
