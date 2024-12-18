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

    % Auxiliary Interface
    close/1,

    % Interfaces 3/3
    addNode/2,
    removeNode/2,
    nodes/1
]).



% Create a new tuple space TS with a specified Name
new(Name) ->
    % Launch the tss module with the init function and the Name of the TS
    spawn(node(), tss, init, [Name]),
    ok
.

% Read a matching Pattern from the tuple space TS (destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple
in(TS, Pattern) -> % Use matching specification
    Ret = in(TS, Pattern, infinity),
    Ret
.

% Read a matching Pattern from the tuple space TS (non-destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple 
rd(TS, Pattern) -> % Use matching specification
    Ret = rd(TS, Pattern, infinity),
    Ret
.

% Write the tuple to the tuple space TS
out(TS, Tuple) -> 
    global:whereis_name(TS)!{out, self(), Tuple},
    ok
.

% Read a matching Pattern from the tuple space TS (destructive) with timeout specification
% if no match is found and the timeout expires, the function returns an error
in(TS, Pattern, Timeout) ->
    % Send the request
    global:whereis_name(TS)!{in, self(), Pattern},

    % Wait for a message 
    receive
        % A match was found 
        {ok, Tuple} -> {ok, Tuple}
    after
        % The timeout has expired
        Timeout ->
            % Send an abort signal to the TS 
            global:whereis_name(TS)!{abort, {in, self(), Pattern}},
            {err, timeout}
    end
.

% Read a matching Pattern from the tuple space TS (non-destructive) with timeout specification
% if no match is found and the timeout expires, the function returns an error 
rd(TS, Pattern, Timeout) ->
    % Send the request
    global:whereis_name(TS)!{rd, self(), Pattern},

    % Wait for a message
    receive
        % A match was found 
        {ok, Tuple} -> {ok, Tuple}
    after
        % The timeout has expired
        Timeout ->
            % Send an abort signal to the TS
            global:whereis_name(TS)!{abort, {rd, self(), Pattern}},
            {err, timeout}
    end
.




% Add the Node to the TS, the node now has access to all tuples int the TS
addNode(TS, Node) ->
    % Send the request for the addition 
    global:whereis_name(TS)!{add_node, self(), Node}
.

% Remove the Node from the TS
removeNode(TS, Node) ->
    % Send the request for the removal
    global:whereis_name(TS)!{rm_node, self(), Node}
.

% Get a list of all nodes that have access to the TS
% the function will return an error if the timeout has expired
nodes(TS) ->
    % Send nodes request
    global:whereis_name(TS)!{nodes, self()},
    receive
        {ok, List} -> List
    after
        5000 -> {err, timeout}
    end
.


% Get a list of all nodes that have access to the TS
% the function will return an error if the timeout has expired
close(TS) ->
    % Send nodes request
    global:whereis_name(TS)!{stop, self()},
    ok
.