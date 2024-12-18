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
init([Name, Supervisor]) ->
    % Enable trap_exit management to handle exit signals properly
    erlang:process_flag(trap_exit, true),
    
    % Obtain reference for tables
    % Create DETS for filesystem synchronization
    {ok, SyncFileRef} = dets:open_file(Name, []),

    % Create an ETS for authorized nodes: whitelist 
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create an ETS for storing tuples: space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Retrieve data from the DETS table into the ETS tuple space
    dets:to_ets(SyncFileRef, TupleSpaceRef),

    % Create WaitQueue for blocking read operations
    WaitQueue = [],

    io:format("Gen Server - START\n", []),

    % Return the initial state of the server with references and empty WaitQueue
    {ok, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.



%% Handle info messages

% Handle the hibernation of the gen server
handle_info(timeout, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    io:format("Gen Server - HYBERNATE\n", []),

    % Periodic saving and synchronization of the tuple space TS with the DETS file
    ets:to_dets(TupleSpaceRef, SyncFileRef),
    dets:sync(SyncFileRef),

    % Hibernate the server 
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}, hibernate};

% Handle 'EXIT' signals 
handle_info({'EXIT', Pid, _Reason}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Pid),
    % Remove the node's pending requests (read operations) from the WaitQueue
    ClearedQueue = removePendingRequests(WaitQueue, Pid),

    % Return the state 
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, ClearedQueue}}
.

%% Handle synchronous endpoints behaviors

% List all tuples in the tuple space TS 
handle_call({list}, _From, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Reply with the list 
    {reply, {ets:tab2list(TupleSpaceRef)}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Check the current WaitQueue  
handle_call({wq}, _From, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Reply with the current state of the WaitQueue
    {reply, {WaitQueue}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Handle the read 'in' operation (destructive) 
handle_call({in, Pattern}, From, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (in) (~p)\n", [Present]),
    case Present of
        % If it's authorized, try the read operation (if there's a match); 
        % otherwise, wait and leave the WaitQueue unmodified 
        true ->
            NewWaitQueue = tryProcessRequest({in, From, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the read 'rd' operation (non-destructive) 
handle_call({rd, Pattern}, From, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (rd) (~p)\n", [Present]),
    case Present of
        % If it's authorized, try the read operation (if there's a match); 
        % otherwise, wait and leave the WaitQueue unmodified 
        true ->
            NewWaitQueue = tryProcessRequest({rd, From, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% List of all the authorized nodes (in the whitelist)
handle_call({nodes}, _From, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Accumulate the nodes in the whitelist by folding over the table
    Acc = ets:foldr(
        fun({Elem}, Acc) ->
            Acc ++ [Elem]	
        end,
        % Initial accumulator (empty list)
        [],
        WhiteListRef
    ),
    
    % Reply with the list 
    {reply, {ok, Acc}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.

%% Handle asynchronous endpoints behaviors 

% Handle the write 'out' operation in the tuple space TS 
handle_cast({out, Pid, Tuple}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        % If it's authorized, insert the tuple in the TS  
        % otherwise, wait and leave the WaitQueue unmodified 
        true ->
            % Insert the tuple 
            ets:insert(TupleSpaceRef, {Tuple}),
            % Process pending requests in the WaitQueue (read operations waiting for a pattern-match)
            NewWaitQueue = processPendingRequests({SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the addition of the Node to the whitelist
handle_cast({add_node, Node}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Link to the Node 
    link(Node),
    % Insert the Node to the whitelist 
    ets:insert(WhiteListRef, {Node}),

    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Handle the removal of the Node from the Whitelist 
handle_cast({rm_node, Node}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Unlink from the Node 
    unlink(Node),
    % Remove the Node from the whitelist
    removeFromWhiteList(WhiteListRef, Node),
    % Remove the Node's read requests from the WaitQueue
    ClearedQueue = removePendingRequests(WaitQueue, Node),

    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, ClearedQueue}};

% Handle the abortion of a request from the WaitQueue 
handle_cast({abort, {Type, Pid, Pattern}}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        % If it's authorized, insert the tuple in the TS  
        % otherwise, wait and leave the WaitQueue unmodified 
        true ->
            % Remove the request from the WaitQueue
            NewWaitQueue = abortPendingRequest({Type, Pid, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    
    {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the simulation of a crash to check the restore of the system  
handle_cast({crash}, State) ->
    exit("Test crash"),
    {noreply, State};

% Handle the stop of the server 
handle_cast({stop, Pid}, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        % If it's authorized, insert the tuple in the TS  
        % otherwise, wait and leave the WaitQueue unmodified 
        true ->
            % Notify the supervisor to delete the server 
            Supervisor!{delete, self()},
            dets:close(SyncFileRef),
            {stop, "Stopped server", {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};
        false ->
            {noreply, {Supervisor, SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
    end
.



%%% Auxiliary functions

% Check if the node is in the whitelist by matching its PID
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        []        -> IsPresent = false;
        [_H | _T] -> IsPresent = true
    end,
    %io:format("DEBUG PRINT - inWhiteList - Node (~p) present? (~p)\n", [Node, IsPresent]),
    IsPresent
.

% Remove the Node from the whitelist 
removeFromWhiteList(WhiteListRef, Node) ->
    ets:delete_object(WhiteListRef, {Node})
.

% Remove pending requests from the WaitQueue referring to a specified Node
removePendingRequests(WaitQueue, Node) ->
    ClearedQueue = lists:foldr(
        fun({_Type, {Pid, _Tag}, _Pattern}, Acc) ->
            case Pid of
                Node -> Acc;
                _ -> Acc ++ [{_Type, {Pid, _Tag}, _Pattern}]
            end
        end,
        [],
        WaitQueue
    ),
    ClearedQueue
.

% Process pending requests in the waitqueue 
processPendingRequests({SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    %io:format("DEBUG PRINT - processPendingRequests (start)\n", []),
    NewWaitQueue = lists:foldr(
		fun({Type, {Pid, Tag}, Pattern}, Acc) -> 
            % Attempt to process a request
			NewAcc = tryProcessRequest({Type, {Pid, Tag}, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, Acc}),
			NewAcc
		end,
		[],
		WaitQueue
	),
    %io:format("DEBUG PRINT - processPendingRequests (end)\n", []),
	NewWaitQueue
.

% Attempt to process a request based on the type (in/rd) and pattern 
tryProcessRequest({Type, {Pid, Tag}, Pattern}, {_SyncFileRef, _WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Control on Pattern Matching
    Res = ets:match_object(TupleSpaceRef, {Pattern}), % MS
    case Res of
        % If no match is found, add the request to the WaitQueue  
        [] ->
            %io:format("DEBUG PRINT - tryProcessRequest (pending)\n", []),
            NewWaitQueue = WaitQueue ++ [{Type, {Pid, Tag}, Pattern}];
        % otherwise process the request  
        [{H} | _T] ->
            %io:format("DEBUG PRINT - tryProcessRequest (matched)\n", []),
            % Reply with the matched tuple 
            gen_server:reply({Pid, Tag}, {ok, H}),
            case Type of
                % Remove the tuple in the 'in' operation (destructive)
                in -> ets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewWaitQueue = WaitQueue
    end,
    %io:format("DEBUG PRINT - tryProcessRequest (ended)\n", []),
    % Return the updated WaitQueue
    NewWaitQueue
.

% Remove a request from the WaitQueue
abortPendingRequest({Type, Pid, Pattern}, {_SyncFileRef, _WhiteListRef, _TupleSpaceRef, WaitQueue}) ->
    NewWaitQueue = lists:foldr(
		fun(Elem, Acc) ->
            %io:format("DEBUG PRINT - abort (~p)\n", [Elem]), 
			case Elem of
                {Type, {Pid, _Tag}, Pattern} ->
                    Acc;
                _ -> Acc ++ [Elem]
            end
		end,
		[],
		WaitQueue
	),
	NewWaitQueue
.