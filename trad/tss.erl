% Tuple Space Supervisor Module Definition
-module(tss).

% Export all invocable functions
-export([
    init/2
]).



% Intialization function 
init(Name, Manager) ->
    % Enable trap_exit management 
    % Setting the flag to trap 'EXIT' signals for handling process crashes or exits
    erlang:process_flag(trap_exit, true),
    
    % Print the supervisor's PID 
    io:format("Supervisor [~p] - Activated\n", [self()]),

    % Start the server for handling incoming messages  
	server(Name, Manager)
.

% Supervisor's server  
server(Name, Manager) ->

    io:format("Supervisor [~p] - Online\n", [self()]),

    % wait for a message
	receive
        % If the tuple space manager process goes down, the supervisor restores it 
		{'EXIT', Manager, _Reason} ->
            
            % Spawn a new process for the 'tsm:init' function and link it to the current process
            NewManager = spawn_link(node(), tsm, init, [Name, self()]),
            % Register the new manager process (it can be accessed globally)
            global:register_name(Name, NewManager),

            % Call the server to continue the loop 
            server(Name, NewManager);
        
        % If the supervisor receives a stop message from the manager, it stops 
        {stop, Manager} ->
            
            % Unlink the process 
            unlink(Manager),
            io:format("Supervisor [~p] - Deactivated\n", [self()]);

        % Handle any other messages  
        _ ->
            server(Name, Manager)
	end
.