% Tuple-Space Manager Module Definition
-module(tsm).

% Export all invocable functions
-export([
    init/2
]).


% Initialization function
init(Name, Supervisor) ->
    % Enable trap_exit management
    erlang:process_flag(trap_exit, true),
    
    %io:format("Debug print - INIT PID (~p)\n", [self()]),

    % Obtain a reference for tables
    % Create DETS for filesystem synchronization
    DetsPath = Name,
    {ok, SyncFileRef} = dets:open_file(DetsPath, []),

    % Create an ETS: whitelist
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create an ETS: space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Retrieve data
    dets:to_ets(SyncFileRef, TupleSpaceRef),

    % Start the server
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

        % Handle ETS destructive read
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
        
        % Handle ETS non-destructive read
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

        % Handle ETS write, PendingRequestsQueue removal
        {out, Pid, Tuple} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            %io:format("Debug print - OUT (~p)\n", [Tuple]),
            %io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
            case Present of 
                % If it's authorized, write the tuple to the TS;
                % otherwise, wait 
                true ->
                    _InsertRes = ets:insert(TupleSpaceRef, {Tuple}),
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                false -> 
                    %io:format("Debug print - OUT - NOT AUTH (~p)\n", [Present]),
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the add node function
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the remove node function 
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
            Pid!{ok},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle the nodes list
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle the closing node function
        {close, Pid} -> 
            % Check if the PID is present in the whitelist (authorized node)
            Present = inWhiteList(WhiteListRef, Pid),
            %io:format("Debug print - OUT (~p)\n", [Tuple]),
            %io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
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

        % test if the TS is restored after a crash (termination of the process)
        {test_crash} ->
            exit("Test exit");

        {list, Pid} ->
            Pid!{okpatato, ets:tab2list(TupleSpaceRef)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {wl, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Wildcard for remove trash messages
        _ -> server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
    
    after
        10000 ->
            ets:to_dets(TupleSpaceRef, SyncFileRef),
            dets:sync(SyncFileRef),
            server(Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
    end
.


% Returns the list of all 'nodes' (Pids) that can access the tuple space
getNodes(WhiteListRef) ->
    F = fun({Elem}, Acc) ->
        Acc ++ [Elem]	
    end,
    Acc = ets:foldr(F, [], WhiteListRef),
    Acc
.

removeNode(WhiteListRef, Node) ->
    unlink(Node), % Not triggering exit?
    removeFromWhiteList(WhiteListRef, Node)
.

removeFromWhiteList(WhiteListRef, Node) ->
    % Remove node from the whitelist
    io:format("Debug print - REMOVE NODE (~p)\n", [Node]),
    ets:delete_object(WhiteListRef, {Node})
.

addNode(WhiteListRef, Node) ->
    % Link the node with the GTS
    link(Node),
    % Insert the node in the whitelist
    ets:insert(WhiteListRef, {Node})
.

% Check if the node is in the whitelist
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        [] -> Present = false;
        [_H | _T]  -> Present = true
    end,
    Present
.

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

tryProcessRequest(TupleSpaceRef, {Type, Pid, Pattern}, PendingRequestsQueue) -> 
    % Control  on Pattern Matching
    Res = ets:match_object(TupleSpaceRef, {Pattern}),
    case Res of
        % If not in the tuple space add to waitqueue
        [] ->
            NewPendingRequestsQueue = PendingRequestsQueue ++ [{Type, Pid, Pattern}];
        % Else return the element 
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