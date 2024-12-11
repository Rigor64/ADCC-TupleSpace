% Tuple Space Behaviors
-module(tsb).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

% Init server state
init({Name}) ->
	% Enable trap_exit management
	erlang:process_flag(trap_exit, true),
	
	%io:format("Debug print - INIT PID (~p)\n", [self()]),

	% Obtain reference for tables
	% Create DETS for filesystem sync
	SyncFileRef = dets:open_file(Name),

	% Create ETS whitelist
	WhiteListRef = ets:new(whitelist, [set, private]),
	% Create ETS space
	TupleSpaceRef = ets:new(space, [set, private]),

	% Create WaitQueue for blocking reads
	WaitQueue = [],

	{ok, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.



% Define sync endpoints behaviors
handle_call({in, Pattern}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_call({rd, Pattern}, From, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

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

% Define async endpoints behaviors
handle_cast({out, Tuple}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({add_node, Node}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({rm_node, Node}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({stop}, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{noreply, {SyncFileRef, WhiteListRef, TupleSpaceRef, WaitQueue}}
.