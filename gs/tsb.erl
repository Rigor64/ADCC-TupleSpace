% Tuple Space Behaviors
-module(tsb).
-behaviour(gen_server).
-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2
]).



%%% Init server state
init([Name]) ->
    % Enable trap_exit management
    erlang:process_flag(trap_exit, true),
    
    % Obtain reference for tables
    % Create DETS for filesystem sync
    {ok, SyncFileRef} = dets:open_file(Name, []),

    % Create ETS whitelist
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create ETS space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Load tuple space
    dets:to_ets(SyncFileRef, TupleSpaceRef),

    % Create WaitQueue for blocking reads
    WaitQueue = [],

    {ok, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.



%%% Handle infos
handle_info(timeout, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Handle dets sync
    ets:to_dets(TupleSpaceRef, SyncFileRef),

    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}, hibernate};

handle_info({'EXIT', Pid, _Reason}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Handle dets sync
    removeFromWhiteList(WhiteListRef, Pid),
    ClearedQueue = removePendingRequests(WaitQueue, Pid),

    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, ClearedQueue}}
.

%%% Define sync endpoints behaviors
%%% TEST
handle_call({list}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {reply, {ets:tab2list(TupleSpaceRef)}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};
handle_call({wq}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {reply, {WaitQueue}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};
%%%
handle_call({in, Pattern}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {Pid, _} = From,
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (in) (~p)\n", [Present]),
    case Present of
        % If autorized try to read and wait otherwise
        true ->
            NewWaitQueue = tryProcessRequest({in, From, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

handle_call({rd, Pattern}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {Pid, _} = From,
    Present = inWhiteList(WhiteListRef, Pid),
    %io:format("DEBUG PRINT - inWhiteList (rd) (~p)\n", [Present]),
    case Present of
        % If autorized try to read and wait otherwise
        true ->
            NewWaitQueue = tryProcessRequest({rd, From, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

% List of connected nodes
handle_call({nodes}, _From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    Acc = ets:foldr(
        fun({Elem}, Acc) ->
            Acc ++ [Elem]	
        end,
        [],
        WhiteListRef
    ),
    
    {reply, {ok, Acc}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.

%%% Define async endpoints behaviors
handle_cast({out, Pid, Tuple}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    Present = inWhiteList(WhiteListRef, Pid),
    case Present of 
        % If autorized try to read and wait otherwise
        true ->
            ets:insert(TupleSpaceRef, {Tuple}),
            NewWaitQueue = processPendingRequests({SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue});
        false ->
            NewWaitQueue = WaitQueue
    end,
    
    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

handle_cast({add_node, Node}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Insert the node in the whitelist
    link(Node),
    ets:insert(WhiteListRef, {Node}),

    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({rm_node, Node}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    unlink(Node),
    removeFromWhiteList(WhiteListRef, Node),
    ClearedQueue = removePendingRequests(WaitQueue, Node),

    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, ClearedQueue}};

handle_cast({abort, {Type, Pid, Pattern}}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    NewWaitQueue = abortPendingRequest({Type, Pid, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}),
    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, NewWaitQueue}};

handle_cast({stop}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    {noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.



%%% Auxiliary functions

inWhiteList(WhiteListRef, Node) ->
    Res = ets:match_object(WhiteListRef, {Node}),
    case Res of
        []        -> IsPresent = false;
        [_H | _T] -> IsPresent = true
    end,
    %io:format("DEBUG PRINT - inWhiteList - Node (~p) present? (~p)\n", [Node, IsPresent]),
    IsPresent
.

removeFromWhiteList(WhiteListRef, Node) ->
    % Delete node from white list
    ets:delete_object(WhiteListRef, {Node})
.

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

processPendingRequests({SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    %io:format("DEBUG PRINT - processPendingRequests (start)\n", []),
    NewWaitQueue = lists:foldr(
		fun({Type, {Pid, Tag}, Pattern}, Acc) -> 
			NewAcc = tryProcessRequest({Type, {Pid, Tag}, Pattern}, {SyncFileRef, WhiteListRef, TupleSpaceRef, Acc}),
			NewAcc
		end,
		[],
		WaitQueue
	),
    %io:format("DEBUG PRINT - processPendingRequests (end)\n", []),
	NewWaitQueue
.

tryProcessRequest({Type, {Pid, Tag}, Pattern}, {_SyncFileRef, _WhiteListRef, TupleSpaceRef, WaitQueue}) ->
    % Control  on Pattern Matching
    Res = ets:match_object(TupleSpaceRef, {Pattern}), % MS
    case Res of
        % If not in the tuple space add to waitqueue
        [] ->
            %io:format("DEBUG PRINT - tryProcessRequest (pending)\n", []),
            NewWaitQueue = WaitQueue ++ [{Type, {Pid, Tag}, Pattern}];
        % Else return the element 
        [{H} | _T] ->
            %io:format("DEBUG PRINT - tryProcessRequest (matched)\n", []),
            gen_server:reply({Pid, Tag}, {ok, H}),
            case Type of
                in -> ets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NewWaitQueue = WaitQueue
    end,
    %io:format("DEBUG PRINT - tryProcessRequest (ended)\n", []),
    NewWaitQueue
.

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