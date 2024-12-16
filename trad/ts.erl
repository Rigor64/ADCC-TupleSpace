% Tuple Space Module Definition
-module(ts).

% Export all invocable functions
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



% Create a new tuple space with a specified Name
new(Name) ->
    spawn(node(), tss, init, [Name]),
    ok
.

% Read a matching Pattern from the tuple space TS (destructive)
% a blocking read operation with no timeout specification 
in(TS, Pattern) -> % Use matching specification
    Ret = in(TS, Pattern, infinity),
    Ret
.

% Read a matching Pattern from the tuple space TS (non-destructive)
% a blocking read operation with no timeout specification 
rd(TS, Pattern) -> % Use matching specification
    Ret = rd(TS, Pattern, infinity),
    Ret
.

% Write Tuple into the tuple space TS
out(TS, Tuple) ->
    global:whereis_name(TS)!{out, self(), Tuple},
    ok
.

% Read a matching Pattern from the tuple space TS (destructive) with timeout specification
% if there's no match and the timeout expires, the function will return an error
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

% Read a matching Pattern from the tuple space TS (non-destructive)
% if there's not a matching and the timeout runs out, then the function returns an error 
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




% Add the Node to the TS, the node now has access to all tuples int the TS
addNode(TS, Node) ->
    % Send in request
    global:whereis_name(TS)!{add_node, self(), Node}
.

% Remove the Node from the TS
removeNode(TS, Node) ->
    % Send in request
    global:whereis_name(TS)!{rm_node, self(), Node}
.

% Get a list of all nodes that have access to the tuple space
% the function will return an error if the timeout is exceeded
nodes(TS) ->
    % Send nodes request
    global:whereis_name(TS)!{nodes, self()},
    receive
        {ok, List} -> List
    after
        5000 -> {err, timeout}
    end
.