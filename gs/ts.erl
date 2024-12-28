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
    % Start a 'gen_server' process for the Tuple Space Manager (tsb)
    gen_server:start({global, Name}, tsb, [Name, true], []),
    % Add the current process to the whitelist
    addNode(Name, self()),
    ok
.

% Read a matching Pattern from the tuple space TS (destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple
% (Use matching specification)
in(TS, Pattern) -> 
    in(TS, Pattern, infinity)
.

% Read a matching Pattern from the tuple space TS (desctructive) with timeout specification
% If no match is found and the timeout expires, the function returns an error message
in(TS, Pattern, Timeout) ->
    
    try 
        % Send a synchronous call to the manager to perform the 'in' operation 
        gen_server:call({global, TS}, {in, Pattern}, Timeout) of
        Response -> Response
    catch
        % Handle timeout error 
        throw:timeout ->
            % Abort the operation by sending an abort message 
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            {err, timeout};

        % Handle other errors that may occur during the 'in' operation
        error: Reason ->
            % Abort the operation by sending an abort message
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            {err, Reason};

        % Handle exit signals
        exit: Reason ->
            % Abort the operation by sending an abort message
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            {Type, _} = Reason,
            % Handle specific exit types
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.

% Read a matching Pattern from the tuple space TS (non-destructive)
% A blocking read operation with no timeout specification,
% which blocks until a match is found and returns the matching tuple 
% (Use matching specification)
rd(TS, Pattern) -> 
    rd(TS, Pattern, infinity)
.

% Read a matching Pattern from the tuple space TS (non-desctructive) with timeout specification
% If no match is found and the timeout expires, the function returns an error message
rd(TS, Pattern, Timeout) ->

    try 
        % Send a synchronous call to the manager to perform the 'rd' operation 
        gen_server:call({global, TS}, {rd, Pattern}, Timeout) of
        Response -> Response
    catch
        % Handle timeout error
        throw:timeout ->
            % Abort the operation by sending an abort message 
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, timeout};

        % Handle other errors that may occur during the 'rd' operation
        error: Reason ->
            % Abort the operation by sending an abort message 
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, Reason};
        
        % Handle exit signals 
        exit: Reason ->
            % Abort the operation by sending an abort message 
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {Type, _} = Reason,
            % Handle specific exit types
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.

% Write the Tuple to the tuple space TS
out(TS, Tuple) ->
    % Send an asynchronous 'out' message 
    gen_server:cast({global, TS}, {out, self(), Tuple})
.

% Add the Node to the whitelist, the Node has access to all tuples in the TS
addNode(TS, Node) ->
    % Send an asynchronous 'add_node' message  
    gen_server:cast({global, TS}, {add_node, self(), Node})
.

% Remove the Node from the whitelist
removeNode(TS, Node) ->
    % Send an asynchronous 'rm_node' message 
    gen_server:cast({global, TS}, {rm_node, self(), Node})
.

% Get list of all nodes that have access to the TS 
nodes(TS) ->
    % Send a synchronous 'nodes' message
    gen_server:call({global, TS}, {nodes})
.

% Stop and close the tuple space
close(TS) ->
    % Send an asynchronous 'stop' message 
    gen_server:cast({global, TS}, {stop, self()}),
    ok
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions for testing purposes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Return the list of tuples in the tuple space TS 
list(TS) -> gen_server:call({global, TS}, {list}).

% Return the wait queue list 
wq(TS) -> gen_server:call({global, TS}, {wq}).

% Send an asunchronous 'crash' message 
% (for ensuring that the system can recover)
crash(TS) -> gen_server:cast({global, TS}, {test_crash, self()}).