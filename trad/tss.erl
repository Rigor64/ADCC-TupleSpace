% Tuple Space Supervisor Module Definition
-module(tss).

% Export all invocable functions
-export([
    init/2
]).



% Intialization function 
% Create a new tuple space manager with a specified Name 
% Enter the server loop to monitor and handle incoming messages 
init(Name, Manager) ->
    erlang:process_flag(trap_exit, true),
    
    % Print the supervisor's PID 
    io:format("Supervisor [~p] - Activated\n", [self()]),
    % Enter the supervisor's loop with manager's PID and reference
	server(Name, Manager)
.

% Main loop of the supervisor for handling messages 
server(Name, Manager) ->
    io:format("Supervisor [~p] - Online\n", [self()]),
    % wait for a message
	receive
        % If the tuple space manager process goes down, the supervisor restarts it 
		{'EXIT', Manager, _Reason} ->
            % Rebuild the manager
            % Spawn and monitor a new process for the tsm which initializes the TS with the specified Name
            NewManager = spawn_link(node(), tsm, init, [Name, self()]),
            % Register the new manager process (it can be accessed globally)
            global:register_name(Name, NewManager),

            % Restart the server loop
            server(Name, NewManager);
        
        % If the supervisor receives a stop message from the manager, it stops 
        {stop, Manager} ->
            unlink(Manager),
            io:format("Supervisor [~p] - Deactivated\n", [self()]);

        _ ->
            server(Name, Manager)
	end
.