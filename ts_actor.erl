-module(ts_actor).
-export([
	init/0
]).


init() ->
	% Obtain reference for tables
	% Create ETS whitelist
	WhiteListRef = ets:new(whitelist, []),
	% Create ETS space
	TupleSpaceRef = ets:new(space, []),
	
	% Start server
	server(WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(WL, TS, WaitQueue) ->
	ok
	%server(WL, TS, WaitQueue)
.