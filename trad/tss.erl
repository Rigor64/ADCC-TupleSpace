% Tuple Space Supervisor Module Definition
-module(tss).

% Export all invocable functions
-export([
    init/2
]).



% Intialization function 
init(Name, Manager) ->
    % Enable trap_exit management
    % Setting the flag to trap 'EXIT' signals for handling exit messages
    erlang:process_flag(trap_exit, true),

    io:format("Supervisor [~p] - Activated\n", [self()]),

    % Start the server for handling incoming messages  
    server(Name, Manager)
.

% Server
server(Name, Manager) ->

    io:format("Supervisor [~p] - Online\n", [self()]),

    % Wait for a message
    receive

        % If the tuple space manager process goes down, the supervisor restores it 
        {'EXIT', Manager, Reason} ->
            {_, Pid} = Reason,

            % Spawn and link a new manager process to the supervisor
            NewManager = spawn_link(node(), tsm, init, [Name, self()]),
            
            % Register the new manager process 
            global:register_name(Name, NewManager),

            Pid!{recovered},

            % Call the server to continue the loop 
            server(Name, NewManager);
        
        % If the supervisor receives a stop message, it stops 
        {stop, Manager} ->
            % Unlink the process 
            unlink(Manager),
            io:format("Supervisor [~p] - Deactivated\n", [self()]);

        % Handle any other message
        _ ->
            server(Name, Manager)
    end
.