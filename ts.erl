-module(ts).
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






% Creates a new tuple space with Name
new(Name) ->
	{todo, Name}
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern) ->
	{todo, TS, Pattern}
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern) ->
	{todo, TS, Pattern}
.

% Write Tuple in the tuple space TS
out(TS, Tuple) ->
	{todo, TS, Tuple}
.






% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern, Timeout) ->
	{todo, TS, Pattern, Timeout}

	% receive
	% 	Pattern -> {ok, Tuple} % Da sistemare palese
	% after
	% 	Timeout -> {err, timeout}
	% end

	% {ok, Tuple}
	% {err, timeout}
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern, Timeout) ->
	{todo, TS, Pattern, Timeout}
	% {ok, Tuple}
	% {err, timeout}
.




% Add Node to the TS, so Node can access to all tuples of TS
addNode(TS, Node) ->
	{todo, TS, Node}
.

% Remove Node from the TS
removeNode(TS, Node) ->
	{todo, TS, Node}
.

nodes(TS) ->
	{todo, TS}
.