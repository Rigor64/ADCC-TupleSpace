% Test Set Module Definition 
-module(tstest).

% Export all invocable functions 
-export([
    avgTimeIN/2,
    avgTimeINonCmd/2,
    avgTimeRD/2,
    avgTimeRDonCmd/2,
    avgTimeOUT/2,
    avgTimeOUTonCmd/2,
	avgTimeRecovery/2,

	testBattery_IO_seq/2,
	testBattery_IO_conc/2
]).


% Measure avg time for the 'in' operation when a start message is received 
avgTimeINonCmd(TS, N) ->
    receive
        {start} -> ok
    end,
	% Measure the average time 
    avgTimeIN(TS, N)
.

% Measure the average time in us for the read destructive ('in') operation for N iterations
avgTimeIN(TS, N) ->
	% Adding the node to the tuple Space TS
    %ts:addNode(TS, self()),

	% The 'in' operation is performed for each element in the sequence
	Times = lists:map(
		fun (E) ->
			% Start timer 
			Tin = erlang:system_time(microsecond),

			% Perform the 'in' operation 
			ts:in(TS, {pattern, E}),

			% Stop timer 
			Tout = erlang:system_time(microsecond),

			% Calculate the elapsed time 
			T = Tout - Tin,
			T
		end,
		% Create a list of integers from 0 to N 
		lists:seq(0, N)
	),
	
	% Total number of iterations 
	Total = length(Times),
	
	% Sum each invidual time calculated 
	Sum = lists:sum(Times),

	% Calculate the average time 
	AvgTime = Sum / Total,

	io:format("Avg time (IN): ~p us\n", [AvgTime])
.

% Measure avg time for the 'rd' operation when a start message is received 
avgTimeRDonCmd(TS, N) ->
    receive
        {start} -> ok
    end,
	% Measure the average time
    avgTimeRD(TS, N)
.

% Measure the average time in us for the read non-destructive ('rd') operation for N iterations
avgTimeRD(TS, N) ->
	% The 'rd' operation is performed for each element in the sequence
	Times = lists:map(
		fun (E) ->
			% Start timer 
			Tin = erlang:system_time(microsecond),

			% Perform the 'rd' operation 
			ts:rd(TS, {pattern, E}),

			% Stop timer
			Tout = erlang:system_time(microsecond),
			
			% Calculate the elapsed time 
			T = Tout - Tin,
			T
		end,
		% Create a list of integers from 0 to N
		lists:seq(0, N)
	),
	
	% Total number of iterations 
	Total = length(Times),
	
	% Sum each individual time 
	Sum = lists:sum(Times),

	% Calculate the average time 
	AvgTime = Sum / Total,

	io:format("Avg time (RD): ~p us\n", [AvgTime])
.

% Measure avg time for the 'out' operation when a start message is received 
avgTimeOUTonCmd(TS, N) ->
    receive
        {start} -> ok
    end,
	% Measure the average time
    avgTimeOUT(TS, N)
.

% Measure the average time in us for the write ('out') operation for N iterations
avgTimeOUT(TS, N) ->
	% Adding the node to the tuple space TS 
    %ts:addNode(TS, self()),

	% The 'out' operation is performed for each element in the sequence
	Times = lists:map(
		fun (E) ->
			% Start timer
			Tin = erlang:system_time(microsecond),

			% Perform the 'out' operation 
			ts:out(TS, {pattern, E}),

			% Stop timer 
			Tout = erlang:system_time(microsecond),

			% Calculate the elapsed time 
			T = Tout - Tin,
			T
		end,
		% Create a list of integers from 0 to N
		lists:seq(0, N)
	),
	
	% Total number of iterations 
	Total = length(Times),
	
	% Sum each invidual time 
	Sum = lists:sum(Times),

	% Calculate the average time 
	AvgTime = Sum / Total,

	io:format("Avg time (OUT): ~p us\n", [AvgTime])
.


% Run sequential average time tests 
testBattery_IO_seq(TS, N) ->
	% Measure and print the average time for the 'out' operation
	avgTimeOUT(TS, N),
	% Measure and print the average time for the 'rd' operation
	avgTimeRD(TS, N),
	% Measure and print the average time for the 'in' operation
	avgTimeIN(TS, N)
.

% Run concurrent average time tests 
testBattery_IO_conc(TS, N) ->
	% Spawn a concurrent process for measuring the average time for the 'out' operation
	OutPid = spawn(tstest, avgTimeOUTonCmd, [TS, N]),
	% Spawn a concurrent process for measuring the average time for the 'in' operation
	InPid = spawn(tstest, avgTimeINonCmd, [TS, N]),

    % Assume to be invoked by a node in whitelist
    ts:addNode(TS, OutPid),
    ts:addNode(TS, InPid),
    
    OutPid!{start},
    InPid!{start}
.

% Measure the average time needed to recovery purposes 
avgTimeRecovery(TS, N) ->
	Times = lists:map(
		fun (_E) ->
			% Start timer 
			Tin = erlang:system_time(microsecond),

            % Crash test call 
			ts:crash(TS),

			% Stop timer 
			Tout = erlang:system_time(microsecond),

			% Calculate the elapsed time 
			T = Tout - Tin,
			T
		end,
		% Create a list of integers from 0 to N
		lists:seq(0, N)
	),
	
	% Total number of iterations 
	Total = length(Times),
	
	% Sum each invidual time 
	Sum = lists:sum(Times),

	% Calculate the average time 
	AvgTime = Sum / Total,

	io:format("Avg recovery time: ~p us\n", [AvgTime])
	
.
	