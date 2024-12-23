% Test Sets Module Definition 
-module(tstest).

% Export all invocable functions 
-export([
    avgTimeIN/2,
    avgTimeRD/2,
    avgTimeOUT/2,
    avgTimeRecovery/2,

    testBattery_IO_seq/2,
    testBattery_IO_conc/2
]).


% Measure the average time in ms for the read destructive (in) operation for N iterations
avgTimeIN(TS, N) ->
    % Adding the node to the tuple Space TS
    ts:addNode(TS, self()),

    % The 'in' operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->

            % Start time 
            Tin = erlang:system_time(microsecond),

            % Perform the 'in' operation 
            ts:in(TS, {pattern, E}),

            % End time 
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

    io:format("Avg time (IN): ~p\n", [AvgTime])
.

% Measure the average time in ms for the read non-destructive ('rd') operation for N iterations
avgTimeRD(TS, N) ->
    % Adding the node to the tuple space TS
    ts:addNode(TS, self()),

    % The 'rd' operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->
            % Start time 
            Tin = erlang:system_time(microsecond),

            % Perform the 'rd' operation 
            ts:rd(TS, {pattern, E}),

            % End time 
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

    io:format("Avg time (RD): ~p\n", [AvgTime])
.

% Measure the average time in ms for the write ('out') operation for N iterations
avgTimeOUT(TS, N) ->
    % Adding the node to the tuple space TS 
    ts:addNode(TS, self()),

    % The 'out' operation is performed for each element in the sequence
    Times = lists:map(
        fun (E) ->
            % Start time 
            Tin = erlang:system_time(microsecond),

            % Perform the 'out' operation 
            ts:out(TS, {pattern, E}),

            % End time 
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

    io:format("Avg time (OUT): ~p\n", [AvgTime])
.


% Run sequential average time tests for in, rd and out operations
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
    % Initiate (spawn) a concurrent process for measuring the average time for the 'out' operation
	spawn(tstest, avgTimeOUT, [TS, N]),
    % Initiate (spawn) a concurrent process for measuring the average time for the 'in' operation
	spawn(tstest, avgTimeIN, [TS, N])
.


avgTimeRecovery(TS, N) ->
	% The 'out' operation is performed for each element in the sequence
	Times = lists:map(
		fun (_E) ->
			% Start time 
			Tin = erlang:system_time(microsecond),

			ts:crash(TS),

			% Wait for a message (response from the TS)
			receive
				% A match was found 
				{recovered} -> ok
			end,

			% End time 
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