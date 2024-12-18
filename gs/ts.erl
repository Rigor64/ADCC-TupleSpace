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
    nodes/1,

    % Auxiliary Interface
    close/1,

    % Testing
    list/1,
    wq/1,
    crash/1
]).



% Creates a new tuple space TS with a specified Name
new(Name) ->
    % Launch the tss module with the init function and the Name of the TS 
    spawn(node(), tss, init, [Name]),
    ok
.



% Read a matching Pattern from the tuple space TS (destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple
in(TS, Pattern) -> % Use matching specification
    in(TS, Pattern, infinity)
.

% Read a matching Pattern from the tuple space TS (desctructive) with timeout specification
% if no match is found and the timeout expires, the function returns an error
in(TS, Pattern, Timeout) ->
    try gen_server:call({global, TS}, {in, Pattern}, Timeout) of
        Response -> Response
    catch
        throw:timeout ->
            %% Handle timeout error specifically
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {err, timeout};
        error: Reason ->
            %% Handle other errors
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {err, Reason};
        exit: Reason ->
            %% Handle exits
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {Type, _} = Reason,
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.



% Read a matching Pattern from the tuple space TS (non-destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple 
rd(TS, Pattern) -> % Use matching specification
    rd(TS, Pattern, infinity)
.

% Read a matching Pattern from the tuple space TS (non-desctructive) with timeout specification
% If no match is found and the timeout expires, the function returns an error 
rd(TS, Pattern, Timeout) ->
    try gen_server:call({global, TS}, {rd, Pattern}, Timeout) of
        Response -> Response
    catch
        throw:timeout ->
            %% Handle timeout error specifically
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, timeout};
        error: Reason ->
            %% Handle other errors
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, Reason};
        exit: Reason ->
            %% Handle exits
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            
            {Type, _} = Reason,
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.









% Write the Tuple to the tuple space TS
out(TS, Tuple) ->
    gen_server:cast({global, TS}, {out, self(), Tuple})
.



% Add the Node to the TS, the Node has access to all tuples in the TS
addNode(TS, Node) ->
    % asynchronous 
    gen_server:cast({global, TS}, {add_node, Node})
.

% Remove the Node from the TS
removeNode(TS, Node) ->
    gen_server:cast({global, TS}, {rm_node, Node})
.

% Get list of all nodes that have access to the TS 
nodes(TS) ->
    gen_server:call({global, TS}, {nodes})
.

% Stop and delete the tuple space
close(TS) ->
    gen_server:cast({global, TS}, {stop, self()}),
    ok
.

%%%
%%%
%%%

% Return the list of tuples in the tuple space TS 
list(TS) -> gen_server:call({global, TS}, {list}).
% Return the wait queue list 
wq(TS) -> gen_server:call({global, TS}, {wq}).

crash(TS) -> gen_server:cast({global, TS}, {crash}).