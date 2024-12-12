% Module definition
-module(ts).

% Export all invokable functions
-export([
    % Interfaces 1/3
    new/1,
    in/2,
    rd/2,
    out/2,

    % Interfaces 2/3
    in/3,
    rd/3,

    % Interfaces 3/3
    addNode/2,
    removeNode/2,
    nodes/1,

    % Testing
    list/1,
    wq/1
]).


%init(Name) ->
%	% Enable trap_exit management
%	erlang:process_flag(trap_exit, true),
%
%	% Start server
%	gen_server:start_link({global, Name}, tsb, [], []),
%
%	supervise(Name)
%.
%
%supervise(Name) ->
%	receive
%		{'EXIT', _Pid, _Reason} ->
%			gen_server:start_link({global, Name}, tsb, [], []),
%			supervise(Name)
%	end
%.


%%%
%%%
%%%

% Creates a new tuple space with Name
new(Name) ->
    gen_server:start({global, Name}, tsb, [atom_to_list(Name)], [{hibernate_after, 10000}]),
    io:format("New tuple space created: ~p\n", [Name]),
    addNode(Name, self())
.



% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern) -> % Use matching specification
    in(TS, Pattern, infinity)
.

% Read Pattern from the tuple space TS (desctructive)
in(TS, Pattern, Timeout) ->
    try gen_server:call({global, TS}, {in, Pattern}, Timeout) of
        Response -> Response
    catch
        throw:timeout ->
            %% Handle timeout error specifically
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {err, timeout};
        error: Reason ->
            %% Handle other errors
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {err, Reason};
        exit: Reason ->
            %% Handle exits
            gen_server:cast({global, TS}, {abort, {in, self(), Pattern}}),
            
            {Type, _} = Reason,
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.



% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern) -> % Use matching specification
    rd(TS, Pattern, infinity)
.

% Read Pattern from the tuple space TS (non-desctructive)
rd(TS, Pattern, Timeout) ->
    try gen_server:call({global, TS}, {rd, Pattern}, Timeout) of
        Response -> Response
    catch
        throw:timeout ->
            %% Handle timeout error specifically
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, timeout};
        error: Reason ->
            %% Handle other errors
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            {err, Reason};
        exit: Reason ->
            %% Handle exits
            gen_server:cast({global, TS}, {abort, {rd, self(), Pattern}}),
            
            {Type, _} = Reason,
            case Type of
                timeout -> {err, timeout};
                _ -> {err, Reason}
            end
    end
.









% Write Tuple in the tuple space TS
out(TS, Tuple) ->
    gen_server:cast({global, TS}, {out, self(), Tuple})
.



% Add Node to the TS, so Node can access to all tuples of TS
addNode(TS, Node) ->
    gen_server:cast({global, TS}, {add_node, Node})
.

% Remove Node from the TS
removeNode(TS, Node) ->
    gen_server:cast({global, TS}, {rm_node, Node})
.

% Get list of nodes who can access to the tuple space
nodes(TS) ->
    gen_server:call({global, TS}, {nodes})
.

%%%
%%%
%%%

list(TS) -> gen_server:call({global, TS}, {list}).
wq(TS) -> gen_server:call({global, TS}, {wq}).