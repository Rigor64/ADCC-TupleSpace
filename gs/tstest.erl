% Test Sets Module Definition 
-module(tstest).

% Export all invocable functions 
-export([
    avgTimeIN/2,
    avgTimeRD/2,
    avgTimeOUT/2,

    testBattery_IO_seq/2,
    testBattery_IO_conc/2
]).


% Measure the average time in ms for the read destructive (in) operation for N iterations
avgTimeIN(TS, N) ->
    % Adding the node to the tuple Space TS
    ts:addNode(TS, self()),

    % The in operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->
            Tin = erlang:system_time(microsecond),
            ts:in(TS, {pattern, E}),
            Tout = erlang:system_time(microsecond),
            T = Tout - Tin,
            T
        end,
        % Create a list of integers from 0 to N 
        lists:seq(0, N)
    ),
    
    Total = length(Times),
    
    Sum = lists:sum(Times),

    AvgTime = Sum / Total,

    io:format("Avg time (IN): ~p\n", [AvgTime])
.

% Measure the average time in ms for the read non-destructive (rd) operation for N iterations
avgTimeRD(TS, N) ->
    % Adding the node to the tuple space TS
    ts:addNode(TS, self()),

    % The rd operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->
            Tin = erlang:system_time(microsecond),
            ts:rd(TS, {pattern, E}),
            Tout = erlang:system_time(microsecond),
            T = Tout - Tin,
            T
        end,
        % Create a list of integers from 0 to N
        lists:seq(0, N)
    ),
    
    Total = length(Times),
    
    Sum = lists:sum(Times),

    AvgTime = Sum / Total,

    io:format("Avg time (RD): ~p\n", [AvgTime])
.

% Measure the average time in ms for the write (out) operation for N iterations
avgTimeOUT(TS, N) ->
    % Adding the node to the tuple space TS 
    ts:addNode(TS, self()),

    % The out operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->
            Tin = erlang:system_time(microsecond),
            ts:out(TS, {pattern, E}),
            Tout = erlang:system_time(microsecond),
            T = Tout - Tin,
            T
        end,
        % Create a list of integers from 0 to N
        lists:seq(0, N)
    ),
    
    Total = length(Times),
    
    Sum = lists:sum(Times),

    AvgTime = Sum / Total,

    io:format("Avg time (OUT): ~p\n", [AvgTime])
.


% Run sequential average time tests for in, rd and out operations
testBattery_IO_seq(TS, N) ->
	avgTimeOUT(TS, N),
	avgTimeRD(TS, N),
	avgTimeIN(TS, N)
.

% Run concurrent average time tests 
testBattery_IO_conc(TS, N) ->
	spawn(tstest, avgTimeOUT, [TS, N]),
	spawn(tstest, avgTimeIN, [TS, N])
.