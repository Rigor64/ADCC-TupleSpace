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



% Creates a new tuple space with Name
new(Name) ->
    global:register_name(Name, spawn(node(), tsm, init, [atom_to_list(Name)])),
    io:format("New tuple space created: ~p\n", [Name]),
    addNode(Name, self()),
    ok
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern) -> % Use matching specification
    Ret = in(TS, Pattern, infinity),
    Ret
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern) -> % Use matching specification
    Ret = rd(TS, Pattern, infinity),
    Ret
.

% Write Tuple in the tuple space TS
out(TS, Tuple) ->
    global:whereis_name(TS)!{out, self(), Tuple},
    ok
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern, Timeout) ->
    % Send in request
    global:whereis_name(TS)!{in, self(), Pattern},

    % Wait for message
    receive
        {ok, Tuple} -> {ok, Tuple}
    after
        Timeout ->
            global:whereis_name(TS)!{abort, {in, self(), Pattern}},
            {err, timeout}
    end
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern, Timeout) ->
    % Send in request
    global:whereis_name(TS)!{rd, self(), Pattern},

    % Wait for message
    receive
        {ok, Tuple} -> {ok, Tuple}
    after
        Timeout ->
            global:whereis_name(TS)!{abort, {rd, self(), Pattern}},
            {err, timeout}
    end
.




% Add Node to the TS, so Node can access to all tuples of TS
addNode(TS, Node) ->
    % Send in request
    global:whereis_name(TS)!{add_node, self(), Node}
.

% Remove Node from the TS
removeNode(TS, Node) ->
    % Send in request
    global:whereis_name(TS)!{remove_node, self(), Node}
.

% Get list of nodes who can access to the tuple space
nodes(TS) ->
    % Send nodes request
    global:whereis_name(TS)!{nodes, self()},
    receive
        {ok, List} -> List
    after
        5000 -> {err, timeout}
    end
.