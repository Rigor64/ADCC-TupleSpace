% Tuple-Space Manager Module Definition
-module(tsm).

% Export all invocable functions
-export([
    init/2
]).



% Initialization function
init(Name, true) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling crashes
    erlang:process_flag(trap_exit, true),

    % Spawn supervisor process
    Supervisor = spawn_link(node(), tss, init, [Name, self()])
    
    % Obtain a reference for tables
    % Create DETS (disk-based table) for filesystem synchronization
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Start the server 
    % (for handling incoming messages for tuple space read/write operations)
    server(Supervisor, WhiteListRef, TupleSpaceRef, []);

% Same as the previous definition, but without spawining the supervisor
init(Name, Supervisor) ->
    erlang:process_flag(trap_exit, true),

    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    WhiteListRef = ets:new(whitelist, [set, private]),

    server(Supervisor, WhiteListRef, TupleSpaceRef, [])
.



% Real TS Server
server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->
    %io:format("Debug print - SERVER PID (~p)\n", [self()]),
    
    receive
        % Handle supervisor crash
        {'EXIT', Supervisor, _Reason} ->
            NewSupervisor = spawn_link(node(), tss, init, [Name, self()]),
            server(NewSupervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle client crash
        {'EXIT', Pid, _Reason} ->
            removeFromWhiteList(WhiteListRef, Pid),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Pid),
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

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
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
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
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Abort a specific in/rd request (the client will call this upon timeout)
        {abort, {Type, Pid, Pattern}} ->  
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, try the read operation (if there's a match); 
                % otherwise, wait 
                true ->
                    NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle ETS write operation for adding the tuple to the TS,
        % PendingRequestsQueue removal
        {out, Pid, Tuple} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, write the tuple to the TS;
                % otherwise, return the same PendingRequestsQueue
                true ->
                    _InsertRes = ets:insert(TupleSpaceRef, {Tuple}),
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                _ ->
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the add node function to the whitelist (authorizing a new node)
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the remove node function from the whitelist (revoking access for a node)
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
            Pid!{ok},
            server(Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the nodes list (retrieve the list of authorized nodes)
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the closing of the tuple space
        {stop, Pid} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If it's authorized, delete the current process and 
                % close the DETS 
                true ->
                    Supervisor!{stop, self()},
                    dets:close(SyncFileRef);
                false ->
                    server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
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
            server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Get a list of the pending requests in the waitqueue
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Check if the PID is in the whitelist (authorized node)
        {wl, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Catch and remove all the unhandled messages (wildcard)
        _ -> server(Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)

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