-module(tstest).
-export([
	testBattery_IO_seq/2,
	testBattery_IO_conc/2
]).

% Module to test
-import(ts).



avgTimeIN(TS, N) ->
	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:in(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(N)
	),
	
	Total = length(Times),
	
	Sum = lists:foldr(
		fun (T, Acc) ->
			NewAcc = Acc + T,
			NewAcc
		end,
		0,
		Times
	),

	AvgTime = Sum / Total,

	io:format("Avg time (IN): ~p", [AvgTime])
.

avgTimeRD(TS, N) ->
	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:rd(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(N)
	),
	
	Total = length(Times),
	
	Sum = lists:foldr(
		fun (T, Acc) ->
			NewAcc = Acc + T,
			NewAcc
		end,
		0,
		Times
	),

	AvgTime = Sum / Total,

	io:format("Avg time (RD): ~p", [AvgTime])
.

avgTimeOUT(TS, N) ->
	Times = lists:map(
		fun (E) ->
			Tin = erlang:system_time(),
			ts:out(TS, {pattern, E}),
			Tout = erlang:system_time(),
			T = Tout - Tin,
			T
		end,
		lists:seq(N)
	),
	
	Total = length(Times),
	
	Sum = lists:foldr(
		fun (T, Acc) ->
			NewAcc = Acc + T,
			NewAcc
		end,
		0,
		Times
	),

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
	spawn(ts, avgTimeOUT, [TS, N]),
	spawn(ts, avgTimeIN, [TS, N])
.