% Tuple Space Behaviors Module Definition 
-module(tsb).

% gen_server behaviour
-behaviour(gen_server).

% Export all invocable functions 
-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2
]).


% Initialization function
init([Name, true]) ->
    % Enable trap_exit management to handle exit signals properly
    erlang:process_flag(trap_exit, true),
    
    % Spawn the supervisor process and link it 
    Supervisor = spawn_link(node(), tss, init, [Name, self()]),

    % Create a DETS (disk-based table) to handle and store the TS
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only the process manager can manage it 
    % - set: no duplicates are allowed 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Empty list for reading operations waiting for a match
    PendingRequestsQueue = [],

    io:format("Gen Server [~p] - Start\n", [self()]),

    % Return the initial state of the server: 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS table for authorized nodes
    % - TupleSpaceRef: reference to the DETS table for the tuples 
    % - PendingRequestsQueue: empty list with pending reading requests
    {ok, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};


% Initialization function with the Supervisor 
init([Name, Supervisor]) ->
    % Enable trap_exit management to handle exit signals properly
    erlang:process_flag(trap_exit, true),
    
    % Create a DETS (disk-based table) to handle and store the TS
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist
    % - private: only the process manager can manage it 
    % - set: no duplicates are allowed  
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Empty list for reading operations waiting for a match
    PendingRequestsQueue = [],

    io:format("Gen Server [~p] - Start\n", [self()]),

    % Return the initial state of the server: 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS for authorized nodes
    % - TupleSpaceRef: reference to the DETS for the tuples 
    % - PendingRequestsQueue: empty list with pending reading requests
    {ok, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
.

%% Handle info messages
% Handle the case when the Supervisor exits 
handle_info({'EXIT', Supervisor, _Reason}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Spawn and link a new Supervisor process 
    NewSupervisor = spawn_link(node(), tss, init, [Name]),
    % Return the updated state of the gen_server
    {noreply, {Name, NewSupervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};

% Handle the case when any other node exits
handle_info({'EXIT', Pid, _Reason}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Pid),
    % Remove the node's pending requests (read operations) from the PendingRequestsQueue
    ClearedQueue = removePendingRequests(PendingRequestsQueue, Pid),
    % Return the updated state 
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, ClearedQueue}}
.


% Handle the list of all tuples in the tuple space TS 
handle_call({list}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Contain the list of tuples 
    Cont = dets:foldr(fun(E, A) -> A ++ [E] end, [], TupleSpaceRef),
    % Reply with the list and the state 
    {reply, {Cont}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};

% Check the current PendingRequestsQueue  
handle_call({wq}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Reply with the current state of the PendingRequestsQueue
    {reply, {PendingRequestsQueue}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};

% Handle the read 'in' operation (destructive) 
handle_call({in, Pattern}, From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of  
        true ->
            % If it's authorized, attempt to process the read operation
            NewPendingRequestsQueue = tryProcessRequest({in, From, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue});
        false ->
            % Otherwise, leave the PendingRequestsQueue unchanged 
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue}};

% Handle the read 'rd' operation (non-destructive) 
handle_call({rd, Pattern}, From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        true ->
            % If it's authorized, attempt to process the read operation
            NewPendingRequestsQueue = tryProcessRequest({rd, From, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue});
        false ->
            % Otherwise, leave the PendingRequestsQueue unchanged
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue}};

% Handle the list of all the authorized nodes (in the whitelist)
handle_call({nodes}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Accumulate the nodes in the whitelist by folding over the table
    Acc = ets:foldr(
        fun({Elem}, Acc) ->
            Acc ++ [Elem]	
        end,
        [],
        WhiteListRef
    ),
    % Reply with the list 
    {reply, {ok, Acc}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
.


% Handle the write 'out' operation in the tuple space TS 
handle_cast({out, Pid, Tuple}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        true ->
            % If it's authorized, insert the tuple in the TS 
            dets:insert(TupleSpaceRef, {Tuple}),
            % Process pending requests in the PendingRequestsQueue (that are waiting for a pattern match)
            NewPendingRequestsQueue = processPendingRequests({Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue});
        false ->
            % Otherwise, leave the PendingRequestsQueue unchanged
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue}};


% Handle the addition of the Node to the whitelist (if it's empty)
handle_cast({add_node, Pid, Pid}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check the first element of the whitelist
    First = ets:first(WhiteListRef),
    case First of
        '$end_of_table' ->
            % If the table is empty, add the node
            addNode(WhiteListRef, Pid),
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};
        _ ->
            % Otherwise, return the status of the server
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
    end;

% Handle the addition of the Node to the whitelist (standard)
handle_cast({add_node, Pid, Node}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check if the PID is present in the whitelist 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                true ->
                    % If it's present, add the node 
                    addNode(WhiteListRef, Node),
                    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};
                false ->
                    % Otherwise, return the status of the server 
                    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
            end;


% Handle the removal of the Node from the Whitelist 
handle_cast({rm_node, Pid, Node}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        true ->
            % If it's present, remove the node 
            removeNode(WhiteListRef, Node),
            % Remove any related pending requests 
            ClearedQueue = removePendingRequests(PendingRequestsQueue, Node),
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, ClearedQueue}};
        false ->
            % Otherwise, return the status of the server 
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
    end;

% Handle the abortion of a request from the PendingRequestsQueue 
handle_cast({abort, {Type, Pid, Pattern}}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of  
        true ->
            % If it's authorized, remove the request from the PendingRequestsQueue
            NewPendingRequestsQueue = abortPendingRequest({Type, Pid, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue});
        false -> 
            % Otherwise, leave the PendingRequestsQueue unchanged 
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewPendingRequestsQueue}};

% Handle the simulation of a crash for resilience purposes
handle_cast({test_crash, Pid}, State) ->
    exit({crash, Pid}),
    {noreply, State};

% Handle the stopping and closing of the TS  
handle_cast({stop, Pid}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        true ->
            % If it's authorized, stop the stystem and close the DETS table
            Supervisor!{stop, self()},
            dets:close(TupleSpaceRef),
            % Stop the server and return the final state 
            {stop, "Stopped server", {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}};
        false ->
            % Otherwise, ignore the request
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}}
    end
.



%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions %
%%%%%%%%%%%%%%%%%%%%%%%

% Check if the node is in the whitelist by matching its PID
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        % If Res is empty (no match is found), the Node is not in the whitelist
        []        -> IsPresent = false;
        % Otherwise, the Node is authorized (in the whitelist)
        [_H | _T] -> IsPresent = true
    end,
    IsPresent
.

% Remove the Node from the whitelist 
removeFromWhiteList(WhiteListRef, Node) ->
    ets:delete_object(WhiteListRef, {Node})
.

% Remove pending requests from the PendingRequestsQueue referring to a specified Node
removePendingRequests(PendingRequestsQueue, Node) ->
    ClearedQueue = lists:foldr(
        fun({_Type, {Pid, _Tag}, _Pattern}, Acc) -> 
            case Pid of
                % If the match is found, do not include it 
                Node -> Acc;
                % Otherwise, include the request in the list 
                _ -> Acc ++ [{_Type, {Pid, _Tag}, _Pattern}]
            end
        end,
        [],
        PendingRequestsQueue
    ),
    % Return the updated queue 
    ClearedQueue
.

% Process pending requests in the PendingRequestsQueue 
processPendingRequests({Name, Supervisor, WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->   
    NewPendingRequestsQueue = lists:foldr(
        fun({Type, {Pid, Tag}, Pattern}, Acc) -> 
            % Attempt to process the request 
            NewAcc = tryProcessRequest({Type, {Pid, Tag}, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, Acc}),
            NewAcc
        end,
        [],
        PendingRequestsQueue
    ),
    % Return the updated PendingRequestsQueue 
    NewPendingRequestsQueue
.

% Attempt to process a request based on the type (in/rd) and pattern 
tryProcessRequest({Type, {Pid, Tag}, Pattern}, {_Name, _Supervisor, _WhiteListRef, TupleSpaceRef, PendingRequestsQueue}) ->
    % Control over Pattern Matching
    Res = dets:match_object(TupleSpaceRef, {Pattern}), 
    case Res of
        % If there's not a match, add the request to the PendingRequestsQueue  
        [] ->
            % in request are appent in tail, while rd are put as head of the queue, allowing full execution of pending requests
            case Type of
                in -> NewPendingRequestsQueue = PendingRequestsQueue ++ [{Type, Pid, Pattern}];
                rd -> NewPendingRequestsQueue = [{Type, Pid, Pattern}] ++ PendingRequestsQueue;
                _ -> NewPendingRequestsQueue = PendingRequestsQueue
            end;
        [{H} | _T] ->
            % Otherwise, process the request
            % Reply with the matched tuple 
            gen_server:reply({Pid, Tag}, {ok, H}),
            case Type of
                % if it's a destructive read, delete the tuple
                in -> dets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewPendingRequestsQueue = PendingRequestsQueue
    end,
    % Return the updated PendingRequestsQueue
    NewPendingRequestsQueue
.

% Remove a request from the PendingRequestsQueue
abortPendingRequest({Type, Pid, Pattern}, {_Name, _Supervisor, _WhiteListRef, _TupleSpaceRef, PendingRequestsQueue}) ->
    NewPendingRequestsQueue = lists:foldr(
        fun(Elem, Acc) ->
            % Check if the current Elem matches the request to abort
            case Elem of
                % If it matches, do not add it 
                {Type, {Pid, _Tag}, Pattern} ->
                    Acc;
                % Otherwise, add it to the new list  
                _ -> Acc ++ [Elem]
            end
        end,
        [],
        PendingRequestsQueue
    ),
    NewPendingRequestsQueue
.

% Remove the given node
removeNode(WhiteListRef, Node) ->
    % Unlink the node 
    unlink(Node), 
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Node)
.

% Add the given node
addNode(WhiteListRef, Node) ->
    % Link the node 
    link(Node),
    % Insert the node to the whitelist
    ets:insert(WhiteListRef, {Node})
.