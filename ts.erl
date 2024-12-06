% Module definition
-module(ts).

% Export all invokable functions
-export([
	% Interfaces 1/3
	new/1,
	in/2,
	rd/2,
	out/2,

	% Interfaces 2/3
	in/3,
	rd/3,

	% Interfaces 3/3
	addNode/2,
	removeNode/2,
	nodes/1
]).

% Import ts_actor
-import(tsm).



% Creates a new tuple space with Name
new(Name) ->
	register(Name, spawn(ts_actor, init, [])),
	io:format("New tuple space created: ~p\n", [Name]),
	addNode(Name, self()),
	ok
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern) ->
	Ret = in(TS, Pattern, infinity),
	Ret
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern) ->
	Ret = rd(TS, Pattern, infinity),
	Ret
.

% Write Tuple in the tuple space TS
out(TS, Tuple) ->
	TS!{out, self(), Tuple},
	ok
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern, Timeout) ->
	% Send in request
	TS!{in, self(), Pattern},

	% Wait for message
	receive
		{ok, Tuple} -> {ok, Tuple}
	after
		Timeout -> {err, timeout}
	end
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern, Timeout) ->
	% Send in request
	TS!{rd, self(), Pattern},

	% Wait for message
	receive
		{ok, Tuple} -> {ok, Tuple}
	after
		Timeout -> {err, timeout}
	end
.




% Add Node to the TS, so Node can access to all tuples of TS
addNode(TS, Node) ->
	% Send in request
	TS!{add_node, Node},
.

% Remove Node from the TS
removeNode(TS, Node) ->
	% Send in request
	TS!{remove_node, Node},
.

% Get list of nodes who can access to the tuple space
nodes(TS) ->
	% Send nodes request
	TS!{nodes, self()}

	% Wait for result
	receive
		{nodes, List} -> todo
	end
.