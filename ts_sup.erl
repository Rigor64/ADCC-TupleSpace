-module(ts_sup).
-export([

]).




init(Name) ->
	build_manager(Name),
	server(Name)
.


server(Name) ->
	receive
		{'DOWN', MonitorRef, process, Object, _Info} -> create_manager(Name), server(Name)
	end
.

build_manager(Name) ->
	{Pid, _Ref} = spawn_monitor(node(), tsm, init, Name),
	global:register_name(Name, Pid)
.