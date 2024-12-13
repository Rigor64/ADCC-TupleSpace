-module(tstest).
-export([
    avgTimeIN/2,
    avgTimeRD/2,
    avgTimeOUT/2,

	testBattery_IO_seq/2,
	testBattery_IO_conc/2
]).



avgTimeIN(TS, N) ->
    ts:addNode(TS, self()),

	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:in(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(0, N)
	),
	
	Total = length(Times),
	
	Sum = lists:sum(Times),

	AvgTime = Sum / Total,

	io:format("Avg time (IN): ~p", [AvgTime])
.

avgTimeRD(TS, N) ->
    ts:addNode(TS, self()),

	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:rd(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(0, N)
	),
	
	Total = length(Times),
	
	Sum = lists:sum(Times),

	AvgTime = Sum / Total,

	io:format("Avg time (RD): ~p", [AvgTime])
.

avgTimeOUT(TS, N) ->
    ts:addNode(TS, self()),

	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:out(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(0, N)
	),
	
	Total = length(Times),
	
	Sum = lists:sum(Times),

	AvgTime = Sum / Total,

	io:format("Avg time (IN): ~p", [AvgTime])
.


% Sequential average time
testBattery_IO_seq(TS, N) ->
	avgTimeOUT(TS, N),
	avgTimeRD(TS, N),
	avgTimeIN(TS, N)
.

% Concurrent average time
testBattery_IO_conc(TS, N) ->
	spawn(tstest, avgTimeOUT, [TS, N]),
	spawn(tstest, avgTimeIN, [TS, N])
.