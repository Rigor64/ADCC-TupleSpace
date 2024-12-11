% Tuple Space Behaviors
-module(tsb).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

% Init server state
init(Args) ->
	% Enable trap_exit management
	erlang:process_flag(trap_exit, true),
	
	%io:format("Debug print - INIT PID (~p)\n", [self()]),

	% Obtain reference for tables
	% Create DETS for filesystem sync
	%SyncFileRef = dets:open_file(),

	% Create ETS whitelist
	WhiteListRef = ets:new(whitelist, [set, private]),
	% Create ETS space
	TupleSpaceRef = ets:new(space, [set, private]),

	% Create WaitQueue for blocking reads
	WaitQueue = [],

	{ok, {WhiteListRef, TupleSpaceRef, WaitQueue}}
.



% Define sync endpoints behaviors
handle_call({in, Pattern}, From, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok, State}, State};

handle_call({rd, Pattern}, From, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok, State}, State};

handle_call({nodes}, From, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok, State}, State}
.

% Define async endpoints behaviors
handle_cast({out, Tuple}, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{noreply, State};

handle_cast({add_node, Node}, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok, State}, {WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({rm_node, Node}, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{reply, {ok, State}, {WhiteListRef, TupleSpaceRef, WaitQueue}};

handle_cast({stop}, {WhiteListRef, TupleSpaceRef, WaitQueue}) ->
	{noreply, {WhiteListRef, TupleSpaceRef, WaitQueue}}
.