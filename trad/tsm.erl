% Tuple-Space Manager module
-module(tsm).
-export([
    init/0
]).

% Initializzation function
init() ->
    % Enable trap_exit management
    erlang:process_flag(trap_exit, true),
    
    %io:format("Debug print - INIT PID (~p)\n", [self()]),

    % Obtain reference for tables
    % Create DETS for filesystem sync
    {ok, SyncFileRef} = dets:open_file("Name", []),

    % Create ETS whitelist
    WhiteListRef = ets:new(whitelist, [set, private]),
    % Create ETS space
    TupleSpaceRef = ets:new(space, [set, private]),

    % Start server
    server(WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue) ->
    %io:format("Debug print - SERVER PID (~p)\n", [self()]),

    receive
        % Handle whitelist removal
        {'EXIT', Pid, _Reason} -> removeFromWhiteList(WhiteListRef, Pid), server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle ETS destructive read
        {in, Pid, Pattern} -> 
            % Check if is in the white list 
            Present = true,%inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    NWQ = tmpFunc(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NWQ = PendingRequestsQueue
            end,	
            server(WhiteListRef, TupleSpaceRef, NWQ);
        
        % Handle ETS non-destructive read
        {rd, Pid, Pattern} -> % Check if is in the white list 
            Present = inWhiteList(WhiteListRef, Pid),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    NWQ = tmpFunc(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue);
                _ -> 
                    NWQ = PendingRequestsQueue
            end, 
            server(WhiteListRef, TupleSpaceRef, NWQ);
        
        % Handle ETS write, PendingRequestsQueue removal
        {out, Pid, Tuple} -> 
            Present = inWhiteList(WhiteListRef, Pid),
            io:format("Debug print - OUT (~p)\n", [Tuple]),
            io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
            case Present of 
                % If autorized try to read and wait otherwise
                true ->
                    _InsertRes = ets:insert(TupleSpaceRef, {Tuple}),
                    NWQ = consumeWQ(TupleSpaceRef, PendingRequestsQueue);
                false -> 
                    io:format("Debug print - OUT - NOT AUTH (~p)\n", [Present]),
                    NWQ = PendingRequestsQueue
            end,
            server(WhiteListRef, TupleSpaceRef, NWQ);

        % Handle add node
        {add_node, Pid, Node} ->
            addNode(WhiteListRef, Node),
            Pid!{ok},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Handle remove node
        {rm_node, Pid, Node} ->
            removeNode(WhiteListRef, Node),
            Pid!{ok},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue); 

        % Handle node list
        {nodes, Pid} ->
            Pid!{ok, getNodes(WhiteListRef)},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Test
        {list, Pid} ->
            Pid!{okpatato, ets:tab2list(TupleSpaceRef)},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {wq, Pid} ->
            Pid!{waitqueue, PendingRequestsQueue},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);
        
        {WhiteListRef, Pid} ->
            Pid!{inWhiteList(WhiteListRef, Pid)},
            server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue);

        % Wildcard for remove trash messages
        _ -> server(WhiteListRef, TupleSpaceRef, PendingRequestsQueue)
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
    io:format("Debug print - Remove Node (func)\n", []),
    unlink(Node), % Not triggering exit?
    removeFromWhiteList(WhiteListRef, Node)
.

removeFromWhiteList(WhiteListRef, Node) ->
    % Remove node from the whitelist
    io:format("Debug print - REMOVE NODE (~p)\n", [Node]),
    ets:delete_object(WhiteListRef, {Node})
.

addNode(WhiteListRef, Node) ->
    % Insert the node in the whitelist
    ets:insert(WhiteListRef, {Node}),
    % Link the node with the GTS
    link(Node)
.

% Check if the node is in the whitelist
inWhiteList(WhiteListRef, Node) ->
%	Res = ets:match(WhiteListRef, ets:fun2ms(matchPid)),
    Res = ets:match_object(WhiteListRef, {Node}),
    %io:format("Debug print - inWhiteList - Node requested (~p)\n", [Node]),
    %io:format("Debug print - inWhiteList - WhiteList content (~p)\n", [ets:tab2list(WhiteListRef)]),
    %io:format("Debug print - inWhiteList - Result of match (~p)\n", [Res]),
    case Res of
        [] -> Present = false;
        [_H | _T]  -> Present = true
    end,
    Present
.

%inFunc(TupleSpaceRef, {in, Pid, Pattern}, PendingRequestsQueue) -> 
%	% Control  on Pattern Matching
%	Res = ets:lookup(TupleSpaceRef, Pattern),
%	case Res of
%		% If not in the tuple space add to waitqueue
%		[] ->
%			NWQ = PendingRequestsQueue ++ {in, Pid, Pattern};
%		% Else return the element and delete the tuple from the tuple space
%		[H | _T] ->
%			Pid!{ok, H},
%			ets:delete(TupleSpaceRef, H),
%			NWQ = PendingRequestsQueue
%	end,
%	NWQ
%.
%
%rdFunc(TupleSpaceRef, {rd, Pid, Pattern}, PendingRequestsQueue) -> 
%	% Control  on Pattern Matching
%	Res = ets:lookup(TupleSpaceRef, Pattern),
%	case Res of
%		% If not in the tuple space add to waitqueue
%		[] ->
%			NWQ = PendingRequestsQueue ++ {rd, Pid, Pattern};
%		% Else return the element 
%		[H | _T] ->
%			Pid!{ok, H},
%			NWQ = PendingRequestsQueue
%	end,
%	NWQ
%.

tmpFunc(TupleSpaceRef, {Type, Pid, Pattern}, PendingRequestsQueue) -> 
    % Control  on Pattern Matching
    Res = ets:match_object(TupleSpaceRef, {Pattern}),
    case Res of
        % If not in the tuple space add to waitqueue
        [] ->
            NWQ = PendingRequestsQueue ++ [{Type, Pid, Pattern}];
        % Else return the element 
        [{H} | _T] ->
            Pid!{ok, H},
            case Type of
                in -> ets:delete_object(TupleSpaceRef, {H});
                rd -> ok
            end,
            NWQ = PendingRequestsQueue
    end,
    NWQ
.

consumeWQ(TupleSpaceRef, WQ) ->
    NWQ = lists:foldr(
        fun({Type, Pid, Pattern}, Acc) -> 
            NewAcc = tmpFunc(TupleSpaceRef, {Type, Pid, Pattern}, Acc),
            NewAcc
        end,
        [],
        WQ	
    ),
    NWQ
.