% Tuple-Space Manager Module Definition
-module(tsm).

% Export all invocable functions
-export([
    init/2
]).


% Initialization function
init(Name, Supervisor) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling crashes
    erlang:process_flag(trap_exit, true),
    
    %io:format("Debug print - INIT PID (~p)\n", [self()]),

    % Obtain a reference for tables
    % Create DETS (disk-based table) for filesystem synchronization
    DetsPath = Name,
    {ok, SyncFileRef} = dets:open_file(DetsPath, []),

    % Create an ETS for authorized nodes: whitelist 
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create an ETS for storing tuples: space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Retrieve data from the DETS table to the ETS tuple space
    dets:to_ets(SyncFileRef, TupleSpaceRef),

    % Start the server 
    %(for handling incoming messages for tuple space read/write operations)
    server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->
    %io:format("Debug print - SERVER PID (~p)\n", [self()]),
    
    receive
        % Handle whitelist removal
        {'EXIT', Pid, _Reason} ->
            removeFromWhiteList(WhiteListRef, Pid),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Pid),
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle ETS destructive read (removes the matching tuple)
        {in, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = true,%inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, try the read operation (if there's a match); 
                % otherwise, wait 
                true ->
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,	
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle ETS non-destructive read (does not remove the matching tuple)
        {rd, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, try the read operation (if there's a match); 
                % otherwise, wait 
                true ->
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Abort a specific in/rd request (the client will call this upon timeout)
        {abort, {Type, Pid, Pattern}} ->  
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, try the read operation (if there's a match); 
                % otherwise, wait 
                true ->
                    NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue);
                false -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle ETS write operation for adding the tuple to the TS,
        % PendingRequestsQueue removal
        {out, Pid, Tuple} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            %io:format("Debug print - OUT (~p)\n", [Tuple]),
            %io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
            case Present of 
                % If it's authorized, write the tuple to the TS;
                % otherwise, return the same PendingRequestsQueue
                true ->
                    _InsertRes = ets:insert(TupleSpaceRef, {Tuple}),
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                false -> 
                    %io:format("Debug print - OUT - NOT AUTH (~p)\n", [Present]),
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the add node function to the whitelist (authorizing a new node)
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the remove node function from the whitelist (revoking access for a node)
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
            Pid!{ok},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the nodes list (retrieve the list of authorized nodes)
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the closing of the tuple space
        {stop, Pid} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, delete the current process and 
                % close the DETS 
                true ->
                    Supervisor!{delete, self()},
                    dets:close(SyncFileRef);
                false ->
                    server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;

        %%%%%%%%
        % Test %
        %%%%%%%%

        % Test to see if the TS is restored after a crash (termination of the process)
        % (Test case to simulate a crash for recovery purposes)
        {test_crash} ->
            exit("Test exit");
        
        % Get a list of the current tuples in the TS 
        {list, Pid} ->
            Pid!{okpatato, ets:tab2list(TupleSpaceRef)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Get a list of the pending requests in the waitqueue
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Check if the PID is in the whitelist (authorized node)
        {wl, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Catch and remove all the unhandled messages (wildcard)
        _ -> server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
    
    after
        10000 ->
            % Periodic saving and synchronization with the DETS (evey 10 secs)
            ets:to_dets(TupleSpaceRef, SyncFileRef),
            dets:sync(SyncFileRef),
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
    end
.


% Return a list of all 'nodes' (PIDs) in the withelist that have access to the TS
getNodes(WhiteListRef) ->
    F = fun({Elem}, Acc) ->
        Acc ++ [Elem]	
    end,
    Acc = ets:foldr(F, [], WhiteListRef),
    Acc
.


removeNode(WhiteListRef, Node) ->
    % Unlink the node 
    unlink(Node), % Not triggering exit?
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Node)
.

% Delete a node from the whitelist
removeFromWhiteList(WhiteListRef, Node) ->
    io:format("Debug print - REMOVE NODE (~p)\n", [Node]),
    ets:delete_object(WhiteListRef, {Node})
.

addNode(WhiteListRef, Node) ->
    % Link the node 
    link(Node),
    % Insert the node to the whitelist
    ets:insert(WhiteListRef, {Node})
.

% Check if the node is in the whitelist by matching its PID
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        [] -> Present = false;
        [_H | _T]  -> Present = true
    end,
    Present
.

% Remove pending requests from the PendingRequestsQueue
removePendingRequests(PendingRequestsQueue, Node) ->
    NewPendingRequestsQueue = lists:foldr(
        fun({_Type, Pid, _Pattern}, Acc) ->
            case Pid of
                Node -> Acc;
                _ -> Acc ++ [{_Type, Pid, _Pattern}]
            end
        end,
        [],
        PendingRequestsQueue
    ),
    NewPendingRequestsQueue
.

% Attempt to process a request based on the type (in/rd) and pattern 
tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, PendingRequestsQueue) -> 
    % Control  on Pattern Matching
    Res = ets:match_object(TupleSpaceRef, {Pattern}),
    case Res of
        % If no match is found, add the request to the PendingRequestsQueue 
        [] ->
            NewPendingRequestsQueue = PendingRequestsQueue ++ [{Type, Pid, Pattern}];
        % otherwise process the request 
        [{H} | _T] ->
            Pid!{ok, H},
            case Type of
                in -> ets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    NewPendingRequestsQueue
.

% Process the pending requests in the waitqueue (WQ)
processPendingRequests(TupleSpaceRef, WQ) ->
    NewPendingRequestsQueue = lists:foldr(
        fun({Type, Pid, Pattern}, Acc) -> 
            NewAcc = tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, Acc),
            NewAcc
        end,
        [],
        WQ	
    ),
    NewPendingRequestsQueue
.

% Remove a request from the PendingRequestsQueue
abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue) ->
    NewPendingRequestsQueue = lists:foldr(
		fun(Elem, Acc) ->
			case Elem of
                {Type, {Pid, _Tag}, Pattern} ->
                    Acc;
                _ -> Acc ++ [Elem]
            end
		end,
		[],
		PendingRequestsQueue
	),
	NewPendingRequestsQueue
.