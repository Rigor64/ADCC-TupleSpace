% Tuple Space Supervisor Module Definition
-module(tss).

% Export all invocable functions
-export([
    init/1
]).



% Intialization function 
% Create a new tuple space manager with a specified Name 
% Enter the server loop to monitor and for handling incoming messages 
init(Name) ->
	{ManagerPid, ManagerRef} = build_manager(Name),
    io:format("Supervisor [~p] - Manager Built\n", [self()]),
	server(Name, ManagerPid, ManagerRef)
.

% Main loop of the supervisor for handling messages 
server(Name, ManagerPid, ManagerRef) ->
    io:format("Supervisor [~p] - ACTIVE\n", [self()]),
    % wait for a message
	receive
        % If the tuple space manager process goes down, the supervisor restart it 
		{'DOWN', _MonitorRef, process, _Object, _Info} ->
            {NewManagerPid, NewManagerRef} = build_manager(Name),
            server(Name, NewManagerPid, NewManagerRef);
        
        % If the supervisor receives a delete message, it stops monitoring 
        {delete, ManagerPid} ->
            demonitor(ManagerRef)
	end
.

% Spawn and monitor a new tuple space manager process
build_manager(Name) ->
    % Spawn a new process for the tsm which initializes the TS with the specified Name
	{Pid, Ref} = spawn_monitor(node(), tsm, init, [atom_to_list(Name), self()]),
	% Register the new manager process (it can be accessed globally)
    global:register_name(Name, Pid),
    io:format("New tuple space created: ~p\n", [Name]),
    % Return the PID and reference of the spawned process
    {Pid, Ref}
.