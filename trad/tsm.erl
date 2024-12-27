% Tuple-Space Manager Module Definition
-module(tsm).

% Export all invocable functions
-export([
    init/2
]).



% Initialization function
init(Name, true) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling exit messages 
    erlang:process_flag(trap_exit, true),

    % Spawn a supervisor process and link it 
    Supervisor = spawn_link(node(), tss, init, [Name, self()]),
    
    % Create a DETS (disk-based table) to handle and store the TS 
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only the process manager can manage it 
    % - set: no duplicates are allowed 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Return the initial state of the server: 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS table for authorized nodes
    % - TupleSpaceRef: reference to the DETS table for storing tuples 
    % - PendingRequestsQueue: empty list with pending reading requests 
    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, []);

% Initialization function 
% (Used when the manager process goes down)
init(Name, Supervisor) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling exit messages 
    erlang:process_flag(trap_exit, true),

    % Create a DETS (disk-based table) to handle and store the TS
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only the process manager can manage it 
    % - set: no duplicates are allowed 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Return the server for handling incoming messages 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS table for authorized nodes
    % - TupleSpaceRef: reference to the DETS table for storing tuples 
    % - PendingRequestsQueue: empty list with pending reading requests 
    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, [])
.



% Server
server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->

    % Wait for a message 
    receive

        % Handle supervisor crash
        {'EXIT', Supervisor, _Reason} ->
            % If an EXIT signal is received, spawn and link a new supervisor 
            NewSupervisor = spawn_link(node(), tss, init, [Name, self()]),
            server(Name, NewSupervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle client crash
        {'EXIT', Pid, _Reason} ->
            % If a node crashes, remove the node from the whitelist 
            removeFromWhiteList(WhiteListRef, Pid),
            % Remove any related pending requests of the removed node
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Pid),
            server(Name,Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle destructive read 
        {in, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's authorized, try processing the read operation
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % Otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,	
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle non-destructive read 
        {rd, Pid, Pattern} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of  
                true ->
                    % If it's authorized, try processing the read operation
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % Otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle the abortion of a specific in/rd request 
        {abort, {Type, Pid, Pattern}} ->  
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's authorized, abort the pending request 
                    NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    % Otherwise, leave the queue unchanged 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle write operation for adding the tuple to the TS
        {out, Pid, Tuple} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's authorized, insert the tuple into the TS 
                    _InsertRes = dets:insert(TupleSpaceRef, {Tuple}),  
                    % Process any pending requests matching the written tuple 
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                _ ->
                    % Otherwise, leave the queue unchanged
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Add the current node to the whitelist (if is empty) 
        {add_node, Pid, Pid} ->
            % Check the first element of the whitelist
            First = ets:first(WhiteListRef),
            case First of
                '$end_of_table' ->
                    % If the table is empty, add the node 
                    addNode(WhiteListRef, Pid),
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
                _ ->
                    % Otherwise, return the status of the server 
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;
        
        % Handle the addition of the Node to the whitelist (standard)
        {add_node, Pid, Node} ->
            % Check if the PID is present in the whitelist 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's present, add the node 
                    addNode(WhiteListRef, Node),
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
                false ->
                    % Otherwise, return the status of the server 
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;


        % Handle the removal of the Node from the whitelist
        {rm_node, Pid, Node} ->
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's present, remove the node 
                    removeNode(WhiteListRef, Node),
                    % Remove any related pending requests 
                    NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
                    Pid!{ok},
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
                false ->
                    % Otherwise, return the status of the server 
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;

        % Return the list  of nodes 
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the closing of the tuple space
        {stop, Pid} -> 
            % Check if the PID is present in the whitelist 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's authorized, delete the current process and close the DETS table
                    Supervisor!{stop, self()},
                    dets:close(TupleSpaceRef);  
                false ->
                    % Otherwise, return the status of the server 
                    server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
            end;


        %%%%%%%%%
        % Tests %
        %%%%%%%%%

        % Test case to simulate a crash for resilience purposes
        {test_crash, Pid} ->
            exit({crash, Pid});
        
        % Get a list of the current tuples in the TS 
        {list, Pid} -> 
            Cont = dets:foldr(fun(E, A) -> A ++ [E] end, [], TupleSpaceRef),
            Pid!{list, Cont}, 
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        % Get a list of the pending reading requests 
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Catch and remove all the unhandled messages (wildcard)
        _ -> server(Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)

    end
.

% Return a list of all nodes in the whitelist that have access to the TS
getNodes(WhiteListRef) ->
    % Scroll the whitelist elements
    Acc = ets:foldr(
        fun({Elem}, Acc) ->
            % Append each element
            Acc ++ [Elem]	
        end,
        [],
        WhiteListRef),
    % Return the list 
    Acc
.

% Remove the given node  
removeNode(WhiteListRef, Node) ->
    % Unlink the node 
    unlink(Node), 
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Node)
.

% Delete a node from the whitelist
removeFromWhiteList(WhiteListRef, Node) ->
    ets:delete_object(WhiteListRef, {Node})
.

% Add the given node 
addNode(WhiteListRef, Node) ->
    % Link the node 
    link(Node),
    % Insert the node to the whitelist
    ets:insert(WhiteListRef, {Node})
.

% Check if the node is in the whitelist 
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        [] -> 
            % If no match is found, set to false
            Present = false; 
        [_H | _T]  -> 
            % If a match is found, set to true 
            Present = true
    end,
    Present
.

% Remove pending requests from the PendingRequestsQueue related to a specific Node 
removePendingRequests(PendingRequestsQueue, Node) ->
    % Create the new list 
    NewPendingRequestsQueue = lists:foldr(
        fun({_Type, Pid, _Pattern}, Acc) -> 
            case Pid of
                Node -> 
                    % If there's a match, do not include related request 
                    Acc;
                _ -> 
                    % Otherwise, include the request in the list     
                    Acc ++ [{_Type, Pid, _Pattern}]
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
        [] ->
            % If there's no match, add the request to the PendingRequestsQueue
            NewPendingRequestsQueue = PendingRequestsQueue ++ [{Type, Pid, Pattern}];
        [{H} | _T] ->
            % Otherwise, process the request 
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

% Process the pending requests in the PendingRequestsQueue
processPendingRequests(TupleSpaceRef, PendingRequestsQueue) ->
    % Process each request 
    NewPendingRequestsQueue = lists:foldr(
        fun({Type, Pid, Pattern}, Acc) -> 
            % Attempt to process a request  
            NewAcc = tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, Acc),
            NewAcc
        end,
        [],
        PendingRequestsQueue	
    ),
    % Return the updated PendingRequestsQueue 
    NewPendingRequestsQueue
.

% Remove a specific request from the PendingRequestsQueue
abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue) ->
    NewPendingRequestsQueue = lists:foldr(
		fun(Elem, Acc) ->
            % Check if the current Elem matches the request to abort
			case Elem of
                {Type, {Pid, _Tag}, Pattern} ->
                    % If it matches, it's not added to the new list 
                    Acc;
                _ -> 
                    % Otherwise, add it to the new list  
                    Acc ++ [Elem]
            end
		end,
		[],
		PendingRequestsQueue
	),
	NewPendingRequestsQueue
.