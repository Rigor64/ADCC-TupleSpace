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
    nodes/1,

    %%% Testing interfaces
    list/1,
    wq/1,
    crash/1
]).



% Create a new tuple space TS with a specified Name
new(Name) ->
    % Spawn the manager process 
    Pid = spawn(node(), tsm, init, [Name, true]),
    % Register the PID (process) globally with the given Name, 
    % allowing other nodes to reference the TS by its Name
    global:register_name(Name, Pid),

    io:format("New tuple space created: ~p\n", [Name]),

    % Add the current node (self()) to the whitelist
    addNode(Name, self()),
    ok
.

% Read a Pattern Matching from the tuple space TS (destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple
% (Use matching specification)
in(TS, Pattern) -> 
    Ret = in(TS, Pattern, infinity),
    Ret
.

% Read a Pattern Matching from the tuple space TS (non-destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple 
% (Use matching specification)
rd(TS, Pattern) -> 
    Ret = rd(TS, Pattern, infinity),
    Ret
.

% Write the tuple to the tuple space TS
out(TS, Tuple) -> 
    global:whereis_name(TS)!{out, self(), Tuple},
    ok
.

% Read a matching Pattern from the tuple space TS (destructive) with timeout specification
% If no match is found and the timeout expires, the function returns an error message 
in(TS, Pattern, Timeout) ->
    % Send request  
    global:whereis_name(TS)!{in, self(), Pattern},

    % Wait for a message (response from the TS)
    receive
        % A match was found 
        {ok, Tuple} -> {ok, Tuple}
    after
        % The timeout has expired
        Timeout ->
            % Send an abort signal 
            global:whereis_name(TS)!{abort, {in, self(), Pattern}},
            % Return an error message indicating that the timeout occured 
            {err, timeout}
    end
.

% Read a matching Pattern from the tuple space TS (non-destructive) with timeout specification
% If no match is found and the timeout expires, the function returns an error message
rd(TS, Pattern, Timeout) ->
    % Send request  
    global:whereis_name(TS)!{rd, self(), Pattern},

    % Wait for a message (response from the TS)
    receive
        % A match was found 
        {ok, Tuple} -> {ok, Tuple}
    after
        % The timeout has expired
        Timeout ->
            % Send an abort signal 
            global:whereis_name(TS)!{abort, {rd, self(), Pattern}},
            % Return an error message indicating that the timeout occured 
            {err, timeout}
    end
.

% Add the Node to the whitelist, the node now has access to all tuples in the TS
addNode(TS, Node) ->
    % Send request 
    global:whereis_name(TS)!{add_node, self(), Node}
.

% Remove the Node from the whitelist
removeNode(TS, Node) ->
    % Send request 
    global:whereis_name(TS)!{rm_node, self(), Node}
.

% Get a list of all nodes that have access to the TS
% The function will return an error message if the timeout has expired
nodes(TS) ->
    % Send request 
    global:whereis_name(TS)!{nodes, self()},

    % Wait for a message (response from the TS)
    receive
        % Return the list of all authorized nodes
        {ok, List} -> List
    after
        % Return an error message indicating that the timeout has expired
        5000 -> {err, timeout}
    end
.

% Close the tuple space TS 
close(TS) ->
    % Send request 
    global:whereis_name(TS)!{stop, self()},
    ok
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Return the list of tuples in the TS 
list(TS) ->
    % Send request 
    global:whereis_name(TS)!{list, self()},

    % Wait for a message (response from the TS)
    receive
        % Return the list of tuples in the TS 
        {list, List} -> List
    after
        % Return an error message indicating that the timeout has expired
        5000 -> {err, timeout}
    end
.

% Return the wait queue list of pending reading requests
wq(TS) ->
    % Send request  
    global:whereis_name(TS)!{wq, self()},

    % Wait for a message (response from the TS)
    receive
        % Return the list of pending requests 
        {waitqueue, List} -> List
    after
        % Return an error message indicating that the timeout has expired
        5000 -> {err, timeout}
    end
.

% Send an asynchronous 'crash' message 
% (for ensuring that the system can recover)
crash(TS) ->
    % Send request 
    global:whereis_name(TS)!{test_crash, self()},

    % Wait for a message (response from the TS)
    receive
        {recovered} -> ok
    end
.