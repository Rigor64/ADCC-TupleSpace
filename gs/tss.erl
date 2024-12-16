-module(tss).
-export([
    init/1
]).



init(Name) ->
	{ManagerPid, ManagerRef} = build_manager(Name),
    io:format("Supervisor [~p] - Manager Built\n", [self()]),
	server(Name, ManagerPid, ManagerRef)
.

server(Name, ManagerPid, ManagerRef) ->
    io:format("Supervisor [~p] - ACTIVE\n", [self()]),
	receive
		{'DOWN', _MonitorRef, process, _Object, _Info} ->
            {NewManagerPid, NewManagerRef} = build_manager(Name),
            server(Name, NewManagerPid, NewManagerRef);
        
        {delete, ManagerPid} ->
            demonitor(ManagerRef)
	end
.

% Spawn and monitor the tuple space manager
build_manager(Name) ->
    {ok, Pid} = gen_server:start({global, Name}, tsb, [atom_to_list(Name), self()], [{hibernate_after, 10000}]),
    Ref = monitor(process, Pid),
    io:format("New tuple space created: ~p\n", [Name]),
    {Pid, Ref}
.