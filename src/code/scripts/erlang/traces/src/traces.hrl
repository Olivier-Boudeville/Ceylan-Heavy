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


% To avoid warnings if not used:
-export([ exec_receive/0 ]).



% For TraceFilename:
-include("traces_defines.hrl").




% Helper function to write receive clauses in simulation cases which cannot
% interfere with trace supervision.
%
% Returns the received value.
%
% Note: exactly as test_receive/0, but with a different name, as all cases are
% not test cases.
%
% Ex: Pid ! {getBaz,[],self()}, MyBaz = exec_receive(), ...
%
exec_receive() ->
	receive
		{wooper_result,V} when V /= monitor_ok ->
			V
	end.
