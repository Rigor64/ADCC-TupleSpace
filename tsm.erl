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
	% Create ETS whitelist
	WhiteListRef = ets:new(whitelist, [set, private]),
	% Create ETS space
	TupleSpaceRef = ets:new(space, [set, private]),

	% Start server
	server(WhiteListRef, TupleSpaceRef, [])
.

% Real TS Server
server(WL, TS, WaitQueue) ->
	%io:format("Debug print - SERVER PID (~p)\n", [self()]),

	receive
		% Handle whitelist removal
		{'EXIT', Pid, _Reason} -> removeFromWhiteList(WL, Pid), server(WL, TS, WaitQueue);

		% Handle ETS destructive read
		{in, Pid, Pattern} -> 
			% Check if is in the white list 
			Present = true,%inWhiteList(WL, Pid),
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
			io:format("Debug print - OUT (~p)\n", [Tuple]),
			io:format("Debug print - OUT - inWhiteList (~p)\n", [Present]),
			case Present of 
				% If autorized try to read and wait otherwise
				true ->
					_InsertRes = ets:insert(TS, {Tuple}),
					NWQ = consumeWQ(TS, WaitQueue);
				false -> 
					io:format("Debug print - OUT - NOT AUTH (~p)\n", [Present]),
					NWQ = WaitQueue
			end,
			server(WL, TS, NWQ);

		% Handle add node
		{add_node, Pid, Node} -> addNode(WL, Node), Pid!{ok}, server(WL, TS, WaitQueue);

		% Handle remove node
		{remove_node, Pid, Node} -> removeNode(WL, Node), Pid!{ok}, server(WL, TS, WaitQueue); 

		% Handle node list
		{nodes, Pid} -> Pid!{ok, getNodes(WL)}, server(WL, TS, WaitQueue);

		% Test
		%{test, Pid} -> Pid!{'ok?'}, ets:insert(WL, {Pid}), server(WL, TS, WaitQueue);
		{list, Pid} -> Pid!{okpatato, ets:tab2list(TS)}, server(WL, TS, WaitQueue);
		{wq, Pid} -> Pid!{waitqueue, WaitQueue}, server(WL, TS, WaitQueue);
		{wl, Pid} -> Pid!{inWhiteList(WL, Pid)}, server(WL, TS, WaitQueue);

		% Wildcard for remove trash messages
		_ -> server(WL, TS, WaitQueue)
	end
.


% Returns the list of all 'nodes' (Pids) that can access the tuple space
getNodes(WL) ->
	F = fun({Elem}, Acc) ->
		Acc ++ [Elem]	
	end,
	Acc = ets:foldr(F, [], WL),
	Acc
.

removeNode(WL, Node) ->
	io:format("Debug print - Remove Node (func)\n", []),
	unlink(Node), % Not triggering exit?
	removeFromWhiteList(WL, Node)
.

removeFromWhiteList(WL, Node) ->
	% Remove node from the whitelist
	io:format("Debug print - REMOVE NODE (~p)\n", [Node]),
	ets:delete_object(WL, {Node})
.
 
addNode(WL, Node) ->
	% Insert the node in the whitelist
	ets:insert(WL, {Node}),
	% Link the node with the GTS
	link(Node)
.

% Check if the node is in the whitelist
inWhiteList(WL, Node) ->
%	Res = ets:match(WL, ets:fun2ms(matchPid)),
	Res = ets:match_object(WL, {Node}),
	%io:format("Debug print - inWhiteList - Node requested (~p)\n", [Node]),
	%io:format("Debug print - inWhiteList - WhiteList content (~p)\n", [ets:tab2list(WL)]),
	%io:format("Debug print - inWhiteList - Result of match (~p)\n", [Res]),
	case Res of
		[] -> Present = false;
		[_H | _T]  -> Present = true
	end,
	Present
.

%inFunc(TS, {in, Pid, Pattern}, WaitQueue) -> 
%	% Control  on Pattern Matching
%	Res = ets:lookup(TS, Pattern),
%	case Res of
%		% If not in the tuple space add to waitqueue
%		[] ->
%			NWQ = WaitQueue ++ {in, Pid, Pattern};
%		% Else return the element and delete the tuple from the tuple space
%		[H | _T] ->
%			Pid!{ok, H},
%			ets:delete(TS, H),
%			NWQ = WaitQueue
%	end,
%	NWQ
%.
%
%rdFunc(TS, {rd, Pid, Pattern}, WaitQueue) -> 
%	% Control  on Pattern Matching
%	Res = ets:lookup(TS, Pattern),
%	case Res of
%		% If not in the tuple space add to waitqueue
%		[] ->
%			NWQ = WaitQueue ++ {rd, Pid, Pattern};
%		% Else return the element 
%		[H | _T] ->
%			Pid!{ok, H},
%			NWQ = WaitQueue
%	end,
%	NWQ
%.

tmpFunc(TS, {Type, Pid, Pattern}, WaitQueue) -> 
	% Control  on Pattern Matching
	Res = ets:match_object(TS, {Pattern}),
	case Res of
		% If not in the tuple space add to waitqueue
		[] ->
			NWQ = WaitQueue ++ [{Type, Pid, Pattern}];
		% Else return the element 
		[{H} | _T] ->
			Pid!{ok, H},
			case Type of
				in -> ets:delete_object(TS, {H});
				rd -> ok
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