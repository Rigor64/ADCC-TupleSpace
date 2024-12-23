% Tuple-Space Manager Module Definition
-module(tsm).

% Export all invocable functions
-export([
    init/2
]).



% Initialization function
init(Name, true) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling process crashes or exits
    erlang:process_flag(trap_exit, true),

    % Spawn a supervisor process to manage the tuple space TS 
    Supervisor = spawn_link(node(), tss, init, [Name, self()]),
    
    % Obtain a reference for tables
    % Create a DETS (disk-based table) to handle and store the TS 
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only accessible to the process creating it 
    % - set: no duplicates are allowed 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Start the server for handling incoming messages 
    % - Name of the TS
    % - Supervisor
    % - Reference to the whitelist ETS table
    % - Reference to the TS DETS
    % - Empty list for the PendingRequestsQueue 
    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, []);

% Initialization function with the Supervisor (no spawning needed)
init(Name, Supervisor) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling process crashes or exits
    erlang:process_flag(trap_exit, true),

    % Obtain a reference for tables
    % Create a DETS (disk-based table) to handle and store the TS 
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only accessible to the process creating it 
    % - set: no duplicates are allowed 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Start the server for handling incoming messages 
    % - Name of the TS
    % - Supervisor
    % - Reference to the whitelist ETS table
    % - Reference to the TS DETS
    % - Empty list for the PendingRequestsQueue 
    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, [])
.



% TS Server
server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->
    %io:format("Debug print - SERVER PID (~p)\n", [self()]),
    
    % Wait for a message 
    receive
        % Handle supervisor crash
        {'EXIT', Supervisor, _Reason} ->
            % If an EXIT signal is received, spawn a new supervisor 
            NewSupervisor = spawn_link(node(), tss, init, [Name, self()]),
            server(Name, NewSupervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle client crash
        {'EXIT', Pid, _Reason} ->
            % If a node crashes, remove the node from the whitelist 
            removeFromWhiteList(WhiteListRef, Pid),
            % Remove any related pending requests of the node removed 
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Pid),
            server(Name,Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle destructive read (removal of the matching tuple)
        {in, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            %inWhiteList(WhiteListRef, Pid),
            Present = true, % Assumption that the PID is authorized 
            case Present of 
                 
                true ->
                    % If it's authorized, try processing the read operation (if there's a match)
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % Otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,	
            server(Name,Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle non-destructive read (does not remove the matching tuple)
        {rd, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of  
                true ->
                    % If it's authorized, try processing the read operation (if there's a match);
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle the abortion of a specific in/rd request (the client will call this upon timeout)
        {abort, {Type, Pid, Pattern}} ->  
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 

                true ->
                    % If it's authorized, abort the pending request 
                    NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle write operation for adding the tuple to the TS
        {out, Pid, Tuple} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 

                true ->
                    % If it's authorized, insert the tuple into the TS (DETS)
                    _InsertRes = dets:insert(TupleSpaceRef, {Tuple}),  
                    % Process any pending requests matching the written tuple 
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                _ ->
                    % otherwise, leave the queue unchanged
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the addition of the Node to the whitelist (authorizing a new node)
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the removal of the Node from the whitelist
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            % Remove any related pending requests 
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
            Pid!{ok},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the nodes list (retrieve the list of authorized nodes)
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the closing of the tuple space
        {stop, Pid} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
        
                true ->
                    % If it's authorized, delete the current process and close the DETS 
                    % Notify the supervisor to stop 
                    Supervisor!{stop, self()},
                    dets:close(TupleSpaceRef);  
                false ->
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;

        %%%%%%%%
        % Test %
        %%%%%%%%

        % Test to see if the TS is restored after a crash (termination of the process)
        % (Test case to simulate a crash for recovery purposes)
        {test_crash, Pid} ->
            exit({crash, Pid});
        
        % Get a list of the current tuples in the TS 
        {list, Pid} ->
            Pid!{okpatato, dets:tab2list(TupleSpaceRef)}, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Get a list of the pending requests in the queue
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Check if the PID is in the whitelist (authorized node)
        {wl, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Catch and remove all the unhandled messages (wildcard)
        _ -> server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)

    end
.


% Return a list of all 'nodes' (PIDs) in the withelist that have access to the TS
getNodes(WhiteListRef) ->
    % Define a function F to accumulate the elements in the ETS 
    F = fun({Elem}, Acc) ->
        % Append each element (Elem) to the accumulator (Acc) list 
        Acc ++ [Elem]	
    end,
    % To traverse the whitelist accumulating the elems 
    Acc = ets:foldr(F, [], WhiteListRef),
    % Return the list 
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
        % If no match is found, Present is set to false
        [] -> Present = false;
        % If a match is found, Res contain a list with the matching Node 
        % (it's in the whitelist)
        [_H | _T]  -> Present = true
    end,
    Present
.

% Remove pending requests from the PendingRequestsQueue for a specific Node 
removePendingRequests(PendingRequestsQueue, Node) ->
    NewPendingRequestsQueue = lists:foldr(
        fun({_Type, Pid, _Pattern}, Acc) ->
            % Check if the request's PID matches the given Node 
            case Pid of
                % If the match is found, do not include it 
                Node -> Acc;
                % otherwise, include the request in the list 
                _ -> Acc ++ [{_Type, Pid, _Pattern}]
            end
        end,
        [],
        PendingRequestsQueue
    ),
    % Return the updated queue
    NewPendingRequestsQueue
.

% Attempt to process a request based on the type (in/rd) and pattern 
tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, PendingRequestsQueue) -> 
    % Control over Pattern Matching
    Res = dets:match_object(TupleSpaceRef, {Pattern}),
    case Res of

        % If no match is found, add the request to the PendingRequestsQueue 
        [] ->
            NewPendingRequestsQueue = PendingRequestsQueue ++ [{Type, Pid, Pattern}];

        % otherwise, process the request 
        [{H} | _T] ->
            Pid!{ok, H},
            case Type of
                % if it's a destructive read, delete the tuple 
                in -> dets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    NewPendingRequestsQueue
.

% Process the pending requests in the WQ (pending requests queue)
processPendingRequests(TupleSpaceRef, WQ) ->
    % Attempt to process each request 
    NewPendingRequestsQueue = lists:foldr(

        fun({Type, Pid, Pattern}, Acc) -> 
            NewAcc = tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, Acc),
            NewAcc
        end,
        [],
        WQ	
    ),
    % Return the updated WQ 
    NewPendingRequestsQueue
.

% Remove a request from the PendingRequestsQueue
abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue) ->
    NewPendingRequestsQueue = lists:foldr(
		fun(Elem, Acc) ->
            % Check if the current Elem matches the request to abort
			case Elem of
                % If it matches, do not add it (removing it from the queue)
                {Type, {Pid, _Tag}, Pattern} ->
                    Acc;
                % otherwise, add it to the accumulator 
                _ -> Acc ++ [Elem]
            end
		end,
		[],
		PendingRequestsQueue
	),
	NewPendingRequestsQueue
.