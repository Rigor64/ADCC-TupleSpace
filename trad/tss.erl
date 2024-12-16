-module(tss).
-export([

]).




init(Name) ->
	ManagerPid = build_manager(Name),
	server(Name, ManagerPid)
.


server(Name, ManagerPid) ->
	receive
		{'DOWN', MonitorRef, process, Object, _Info} -> NewManagerPid = build_manager(Name), server(Name, NewManagerPid),
        {delete, ManagerPid}
	end
.

build_manager(Name) ->
	{Pid, _Ref} = spawn_monitor(node(), tsm, init, Name),
	global:register_name(Name, Pid)
.