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

    % Obtain reference for tables
    % Create DETS for filesystem synchronization
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Create WaitQueue for blocking read operations waiting for a match
    WaitQueue = [],

    io:format("Gen Server [~p] - Start\n", [self()]),

    % Return the initial state of the server: 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS for authorized nodes
    % - TupleSpaceRef: reference to the DETS for the tuples 
    % - WaitQueue: empty list with pending reading requests
    {ok, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};


% Initialization function with the Supervisor 
init([Name, Supervisor]) ->

    % Enable trap_exit management to handle exit signals properly
    erlang:process_flag(trap_exit, true),
    
    % Obtain reference for tables
    % Create DETS for filesystem synchronization
    DetsPath = atom_to_list(Name),
    {ok, TupleSpaceRef} = dets:open_file(DetsPath, [{auto_save, 60000}, {ram_file, true}]),

    % Create an ETS for authorized nodes: whitelist 
    WhiteListRef = ets:new(whitelist, [set, private]),

    % Create WaitQueue for blocking read operations waiting for a match 
    WaitQueue = [],

    io:format("Gen Server [~p] - Start\n", [self()]),

    % Return the initial state of the server: 
    % - Name: the name of the Tuple Space TS 
    % - Supervisor: the PID of the supervisor 
    % - WhiteListRef: reference to the ETS for authorized nodes
    % - TupleSpaceRef: reference to the DETS for the tuples 
    % - WaitQueue: empty list with pending reading requests
    {ok, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}}
.



%% Handle info messages
% Handle the case when the Supervisor exits 
handle_info({'EXIT', Supervisor, _Reason}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    
    % Spawn and link a new Supervisor process 
    NewSupervisor = spawn_link(node(), tss, init, [Name]),

    % Return the updated state of the gen_server
    {noreply, {Name, NewSupervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Handle the case when any other node exits
handle_info({'EXIT', Pid, _Reason}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    
    % Remove the node from the whitelist 
    removeFromWhiteList(WhiteListRef, Pid),
    
    % Remove the node's pending requests (read operations) from the WaitQueue
    ClearedQueue = removePendingRequests(WaitQueue, Pid),

    % Return the updated state 
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, ClearedQueue}}
.


%% Handle synchronous endpoints behaviors

% Handle the list of all tuples in the tuple space TS 
handle_call({list}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Contain the list of tuples 
    Cont = dets:foldr(fun(E, A) -> A ++ [E] end, [], TupleSpaceRef),
    
    % Reply with the list and the state 
    {reply, {Cont}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Check the current WaitQueue  
handle_call({wq}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Reply with the current state of the WaitQueue
    {reply, {WaitQueue}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Handle the read 'in' operation (destructive) 
handle_call({in, Pattern}, From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (in) (~p)\n", [Present]),
    case Present of

        % If it's authorized, attempt to process the read operation  
        true ->
            NewWaitQueue = tryProcessRequest({in, From, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue});
         % otherwise, leave the WaitQueue unchanged 
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the read 'rd' operation (non-destructive) 
handle_call({rd, Pattern}, From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {Pid, _} = From,
    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (rd) (~p)\n", [Present]),
    case Present of

        % If it's authorized, attempt to process the read operation 
        true ->
            NewWaitQueue = tryProcessRequest({rd, From, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue});
        % otherwise, leave the WaitQueue unchanged
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the list of all the authorized nodes (in the whitelist)
handle_call({nodes}, _From, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Accumulate the nodes in the whitelist by folding over the table
    Acc = ets:foldr(
        fun({Elem}, Acc) ->
            Acc ++ [Elem]	
        end,
        [],
        WhiteListRef
    ),
    
    % Reply with the list 
    {reply, {ok, Acc}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}}
.

%% Handle asynchronous endpoints behaviors 

% Handle the write 'out' operation in the tuple space TS 
handle_cast({out, Pid, Tuple}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
          
        % If it's authorized, insert the tuple in the TS
        true ->
            % Insert the tuple 
            dets:insert(TupleSpaceRef, {Tuple}),
            % Process pending requests in the WaitQueue (that are waiting for a pattern match)
            NewWaitQueue = processPendingRequests({Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue});

        % otherwise, leave the WaitQueue unchanged  
        false ->
            NewWaitQueue = WaitQueue
    end,
    
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the addition of the Node to the whitelist
handle_cast({add_node, Node}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Link the Node 
    link(Node),
    % Insert the Node to the whitelist 
    ets:insert(WhiteListRef, {Node}),

    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};

% Handle the removal of the Node from the Whitelist 
handle_cast({rm_node, Node}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Unlink the Node 
    unlink(Node),
    % Remove the Node from the whitelist
    removeFromWhiteList(WhiteListRef, Node),
    % Remove the Node's read requests from the WaitQueue
    ClearedQueue = removePendingRequests(WaitQueue, Node),

    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, ClearedQueue}};

% Handle the abortion of a request from the WaitQueue 
handle_cast({abort, {Type, Pid, Pattern}}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
         
        % If it's authorized, insert the tuple in the TS 
        true ->
            % Remove the request from the WaitQueue
            NewWaitQueue = abortPendingRequest({Type, Pid, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue});
        % otherwise, leave the WaitQueue unchanged 
        false -> 
            NewWaitQueue = WaitQueue
    end,
    
    {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% Handle the simulation of a crash to check the system's recovery
handle_cast({crash}, State) ->
    exit("Test crash"),
    {noreply, State};

% Handle the stop of the server 
handle_cast({stop, Pid}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->

    % Check if the PID is present in the whitelist (authorized node)
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
          
        % If it's authorized, insert the tuple in the TS
        true ->
            % Notify the supervisor to stop 
            Supervisor!{stop, self()},
            % Close the TupleSpaceRef
            dets:close(TupleSpaceRef),
            % Stop the server and return the final state 
            {stop, "Stopped server", {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}};
        % otherwise, ignore the request
        false ->
            {noreply, {Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}}
    end
.



%%% Auxiliary functions

% Check if the node is in the whitelist by matching its PID
inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        % If Res is empty (no match is found), the Node is not in the whitelist
        []        -> IsPresent = false;
        % otherwise, the Node is authorized (in the whitelist)
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
            % Check if the request's PID matches the given Node 
            case Pid of
                % If the match is found, do not include it 
                Node -> Acc;
                % otherwise, include the request in the list 
                _ -> Acc ++ [{_Type, {Pid, _Tag}, _Pattern}]
            end
        end,
        [],
        WaitQueue
    ),
    % Return the updated queue 
    ClearedQueue
.

% Process pending requests in the WaitQueue 
processPendingRequests({Name, Supervisor, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    %io:format("DEBUG PRINT - processPendingRequests (start)\n", []),
    
    NewWaitQueue = lists:foldr(
		fun({Type, {Pid, Tag}, Pattern}, Acc) -> 
            % Attempt to process the request 
			NewAcc = tryProcessRequest({Type, {Pid, Tag}, Pattern}, {Name, Supervisor, WhiteListRef, TupleSpaceRef, Acc}),
			NewAcc
		end,
		[],
		WaitQueue
	),
    %io:format("DEBUG PRINT - processPendingRequests (end)\n", []),
    % Return the updated WaitQueue 
	NewWaitQueue
.

% Attempt to process a request based on the type (in/rd) and pattern 
tryProcessRequest({Type, {Pid, Tag}, Pattern}, {_Name, _Supervisor, _WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Control on Pattern Matching
    Res = dets:match_object(TupleSpaceRef, {Pattern}), % MS
    case Res of
        % If no match is found, add the request to the WaitQueue  
        [] ->
            %io:format("DEBUG PRINT - tryProcessRequest (pending)\n", []),
            NewWaitQueue = WaitQueue ++ [{Type, {Pid, Tag}, Pattern}];

        % otherwise, process the request  
        [{H} | _T] ->
            %io:format("DEBUG PRINT - tryProcessRequest (matched)\n", []),
            % Reply with the matched tuple 
            gen_server:reply({Pid, Tag}, {ok, H}),

            case Type of
                % if it's a destructive read, delete the tuple
                in -> dets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewWaitQueue = WaitQueue
    end,
    %io:format("DEBUG PRINT - tryProcessRequest (ended)\n", []),
    % Return the updated WaitQueue
    NewWaitQueue
.

% Remove a request from the WaitQueue
abortPendingRequest({Type, Pid, Pattern}, {_Name, _Supervisor, _WhiteListRef, _TupleSpaceRef, WaitQueue}) ->

    NewWaitQueue = lists:foldr(
		fun(Elem, Acc) ->
            %io:format("DEBUG PRINT - abort (~p)\n", [Elem]), 
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
		WaitQueue
	),
	NewWaitQueue
.