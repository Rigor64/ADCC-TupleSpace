% Tuple-Space Manager module
-module(tsm).
-export([
	init/0
]).

% Initializzation function
init() ->
	% Enable trap_exit management
	erlang:process_flag(trap_exit, true),
	
	% Obtain reference for tables
	% Create ETS whitelist
	WhiteListRef = ets:new(whitelist, [set, private]),
	% Create ETS space
	TupleSpaceRef = ets:new(space, [duplicate_bag, private]),

	% Start server
	server(WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(WL, TS, WaitQueue) ->
	receive
		% Handle whitelist removal
		{'EXIT', Pid, _Reason} -> removeFromWhiteList(WL, Pid), server(WL, TS, WaitQueue);

		% Handle ETS destructive read
		{in, Pid, Pattern} -> 
			% Check if is in the white list 
			Present = inWhiteList(WL, Pid),
			case Present of 
				% If autorized try to read and wait otherwise
				true ->
					NWQ = tmpFunc(TS, {in, Pid, Pattern}, WaitQueue);
				_ -> 
					NWQ = WaitQueue
			end,	
			server(WL, TS, NWQ);
		
		% Handle ETS non-destructive read
		{rd, Pid, Pattern} -> % Check if is in the white list 
			Present = inWhiteList(WL, Pid),
			case Present of 
				% If autorized try to read and wait otherwise
				true ->
					NWQ = tmpFunc(TS, {rd, Pid, Pattern}, WaitQueue);
				_ -> 
					NWQ = WaitQueue
			end, 
			server(WL, TS, NWQ);
		
		% Handle ETS write, WaitQueue removal
		{out, Pid, Tuple} -> 
			Present = inWhiteList(WL, Pid),
			case Present of 
				% If autorized try to read and wait otherwise
				true ->
					ets:insert(TS, Tuple),
					NWQ = consumeWQ(TS, WaitQueue);
				_ -> 
					NWQ = WaitQueue
			end, 
			server(WL, TS, NWQ);

		% Handle add node
		{add_node, Pid, Node} -> addNode(WL, Node), Pid!{ok, "Il nodo ~p è stato aggiunto", Node}, server(WL, TS, WaitQueue);

		% Handle remove node
		{remove_node, Pid, Node} -> removeNode(Node), Pid!{ok, "Il nodo ~p è stato rimosso", Node}, server(WL, TS, WaitQueue);
		% Ritorna un messaggio {'EXIT', Pid, _ } 

		% Handle node list
		{nodes, Pid} -> Pid!{ok, getNodes(WL)}, server(WL, TS, WaitQueue);

		% Wildcard for remove trash messages
		_ -> server(WL, TS, WaitQueue)
	end
.


getNodes(WL) ->
	F = fun(Elem, Acc) ->
		Acc ++ Elem	
	end,
	Acc = ets:foldr(F, [], WL),
	Acc
.

removeNode(Node) ->
	unlink(Node)
.

removeFromWhiteList(WL, Node) ->
	% Remove node from the whitelist
	ets:delete(WL, Node)
.
 
addNode(WL, Node) ->
	% Insert the node in the whitelist
	ets:insert(WL, Node),
	% Link the node with the GTS
	link(Node)
.

% Check if the node is in the whitelist
inWhiteList(WL, Node) ->
	Res = ets:lookup(WL, Node),
	case Res of
		[] -> false;
		_  -> true
	end 
.


inFunc(TS, {in, Pid, Pattern}, WaitQueue) -> 
	% Control  on Pattern Matching
	Res = ets:lookup(TS, Pattern),
	case Res of
		% If not in the tuple space add to waitqueue
		[] ->
			NWQ = WaitQueue ++ {in, Pid, Pattern};
		% Else return the element and delete the tuple from the tuple space
		[H | _T] ->
			Pid!{ok, H},
			ets:delete(TS, H),
			NWQ = WaitQueue
	end,
	NWQ
.

rdFunc(TS, {rd, Pid, Pattern}, WaitQueue) -> 
	% Control  on Pattern Matching
	Res = ets:lookup(TS, Pattern),
	case Res of
		% If not in the tuple space add to waitqueue
		[] ->
			NWQ = WaitQueue ++ {rd, Pid, Pattern};
		% Else return the element 
		[H | _T] ->
			Pid!{ok, H},
			NWQ = WaitQueue
	end,
	NWQ
.

tmpFunc(TS, {Type, Pid, Pattern}, WaitQueue) -> 
	% Control  on Pattern Matching
	Res = ets:lookup(TS, Pattern),
	case Res of
		% If not in the tuple space add to waitqueue
		[] ->
			NWQ = WaitQueue ++ {Type, Pid, Pattern};
		% Else return the element 
		[H | _T] ->
			Pid!{ok, H},
			case Type of
				in -> ets:delete(TS, H);
				_ -> ok
			end,
			NWQ = WaitQueue
	end,
	NWQ
.

consumeWQ(TS, WQ) ->
	NWQ = lists:foldr(
		fun({Type, Pid, Pattern}, Acc) -> 
			NewAcc = tmpFunc(TS, {Type, Pid, Pattern}, Acc),
			NewAcc
		end,
		[],
		WQ	
	),
	NWQ
.