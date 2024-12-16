% Tuple-Space Manager module
-module(tsm).
-export([
    init/1
]).

% Initializzation function
init(Name) ->
    % Enable trap_exit management
    erlang:process_flag(trap_exit, true),
    
    %io:format("Debug print - INIT PID (~p)\n", [self()]),

    % Obtain reference for tables
    % Create DETS for filesystem sync
    DetsPath = Name,
    {ok, SyncFileRef} = dets:open_file(DetsPath, []),

    % Create ETS whitelist
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create ETS space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Start server
    server(SyncFileRef, WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->
    %io:format("Debug print - SERVER PID (~p)\n", [self()]),

    receive
        % Handle whitelist removal
        {'EXIT', Pid, _Reason} ->
            removeFromWhiteList(WhiteListRef, Pid),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Pid),
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle ETS destructive read
        {in, Pid, Pattern} -> 
            % Check if is in the white list 
            Present = true,%inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,	
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Handle ETS non-destructive read
        {rd, Pid, Pattern} -> % Check if is in the white list 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    NewPendingRequestsQueue = tryProcessRequest(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);
        
        % Abort a specific in/rd request (upon timeout, client will call this)
        {abort, {Type, Pid, Pattern}} -> % Check if is in the white list 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, PendingRequestsQueue);
                false -> 
                    NewPendingRequestsQueue = PendingRequestsQueue
            end, 
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle ETS write, PendingRequestsQueue removal
        {out, Pid, Tuple} -> 
            Present = inWhiteList(WhiteListRef, Pid),
            %io:format("Debug print - OUT (~p)\n", [Tuple]),
            %io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    _InsertRes = ets:insert(TupleSpaceRef, {Tuple}),
                    NewPendingRequestsQueue = processPendingRequests(TupleSpaceRef, PendingRequestsQueue);
                false -> 
                    %io:format("Debug print - OUT - NOT AUTH (~p)\n", [Present]),
                    NewPendingRequestsQueue = PendingRequestsQueue
            end,
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle add node
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle remove node
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            NewPendingRequestsQueue = removePendingRequests(PendingRequestsQueue, Node),
            Pid!{ok},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue);

        % Handle node list
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        %%%%%%%%
        % Test %
        %%%%%%%%
        {list, Pid} ->
            Pid!{okpatato, ets:tab2list(TupleSpaceRef)},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {wl, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Wildcard for remove trash messages
        _ -> server(SyncFileRef, WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
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