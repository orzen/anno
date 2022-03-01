-module(anno_proc).

% Unused
-export(
  [
    suitable_by_load/1,
    suitable_by_module/1,
    suitable_by_labels/1,
    spawn1/0,
    start/0,
    start_link/0,
    start_monitor/0,
    select_node/2
  ]
).
-export(
  [
    gen_server_start/3,
    gen_server_start/4,
    gen_server_start_link/3,
    gen_server_start_link/4,
    gen_server_start_monitor/3,
    gen_server_start_monitor/4
  ]
).

-define(PROC, proc_lib).

spawn1() -> ok.

start() -> ok.

start_link() -> ok.

start_monitor() -> ok.

% Use os_mon to determine if CPU, mem, disk is sufficient to use the node.
-spec suitable_by_load(Nodes :: list()) -> list().
suitable_by_load(_Nodes) -> ok.

% Check which labels that are associated with a particular module and then
% check which nodes that is allowed to host the any of the labels.
-spec suitable_by_module(Module :: atom()) -> list().
suitable_by_module(_Module) -> ok.

% Labels specified in anno_opts will override module associated labels.
-spec suitable_by_labels(Labels :: list()) -> list().
suitable_by_labels(_Labels) -> ok.

select_node(_Module, _Labels) -> ok.

% Inspired by proc_lib:kill_flush/1
kill_pid(Pid) ->
  unlink(Pid),
  exit(Pid, kill),
  receive {'EXIT', Pid, _} -> ok after 0 -> ok end.


% Inspired by gen:timeout/1
with_timeout(Options) ->
  case lists:keyfind(timeout, 1, Options) of
    {_, Timeout} -> Timeout;
    false -> infinity
  end.


% Inspired by gen:spawn_opts/1
with_spawn_opt(Options) ->
  case lists:keyfind(spawn_opt, 1, Options) of
    {_, Opts} -> Opts;
    false -> []
  end.


with_anno_opt(Options) ->
  case lists:keyfind(anno_opt, 1, Options) of
    {_, Opts} -> Opts;
    false -> []
  end.


% Inspired by gen:monitor_return/1
return_monitor({{ok, Pid}, Monitor}) when is_pid(Pid), is_reference(Monitor) ->
  {ok, {Pid, Monitor}};

return_monitor({Error, Monitor}) when is_reference(Monitor) ->
  receive {'DOWN', Monitor, process, _Pid, _Reason} -> Error end.

% Functionality corresponding to the one in stdlib gen_server.erl
% Without Name use `associate` to check nodes
gen_server_start(Mod, Args, Opts) -> gen_start(gen_server, Mod, Args, Opts).

gen_server_start(Name, Mod, Args, Opts) -> gen_start(gen_server, Name, Mod, Args, Opts).

gen_server_start_link(Mod, Args, Opts) -> gen_start_link(gen_server, Mod, Args, Opts).

gen_server_start_link(Name, Mod, Args, Opts) -> gen_start_link(gen_server, Name, Mod, Args, Opts).

gen_server_start_monitor(Mod, Args, Opts) -> gen_start_monitor(gen_server, Mod, Args, Opts).

gen_server_start_monitor(Name, Mod, Args, Opts) ->
  gen_start_monitor(gen_server, Name, Mod, Args, Opts).

% Functionality corresponding to the one in stdlib gen.erl
gen_start_link(GenMod, Mod, Args, Opts) ->
  proc_start_link(
    gen,
    init_it,
    [GenMod, self(), self(), Mod, Args, Opts],
    with_timeout(Opts),
    with_spawn_opt(Opts),
    with_anno_opt(Opts)
  ).

gen_start_link(GenMod, Name, Mod, Args, Opts) ->
  proc_start_link(
    gen,
    init_it,
    [GenMod, self(), self(), Name, Mod, Args, Opts],
    with_timeout(Opts),
    with_spawn_opt(Opts),
    with_anno_opt(Opts)
  ).

gen_start_monitor(GenMod, Mod, Args, Opts) ->
  Ret =
    proc_start_monitor(
      gen,
      init_it,
      [GenMod, self(), self(), Mod, Args, Opts],
      with_timeout(Opts),
      with_spawn_opt(Opts),
      with_anno_opt(Opts)
    ),
  return_monitor(Ret).


gen_start_monitor(GenMod, Name, Mod, Args, Opts) ->
  Ret =
    proc_start_monitor(
      gen,
      init_it,
      [GenMod, self(), self(), Name, Mod, Args, Opts],
      with_timeout(Opts),
      with_spawn_opt(Opts),
      with_anno_opt(Opts)
    ),
  return_monitor(Ret).


gen_start(GenMod, Mod, Args, Opts) ->
  proc_start(
    gen,
    init_it,
    [GenMod, self(), self(), Mod, Args, Opts],
    with_timeout(Opts),
    with_spawn_opt(Opts),
    with_anno_opt(Opts)
  ).

gen_start(GenMod, Name, Mod, Args, Opts) ->
  proc_start(
    gen,
    init_it,
    [GenMod, self(), self(), Name, Mod, Args, Opts],
    with_timeout(Opts),
    with_spawn_opt(Opts),
    with_anno_opt(Opts)
  ).

-spec ensure_no_monitor(M :: atom(), F :: atom(), A :: list(), T :: integer(), Opts :: list()) ->
  ok | no_return().
ensure_no_monitor(M, F, A, T, Opts) ->
  case list:members(monitor, Opts) of
    true -> erlang:error(badarg, [M, F, A, T, Opts]);
    false -> ok
  end.


% Inspired by proc_lib:sync_start/2
eval_start(Pid, Ref, Timeout) ->
  receive
    {ack, Pid, Return} ->
      erlang:demonitor(Ref, [flush]),
      Return;

    {'DOWN', Ref, process, Pid, Reason} -> {error, Reason}
  after
    Timeout ->
      erlang:demonitor(Ref, [flush]),
      kill_pid(Pid),
      {error, timeout}
  end.


% Inspired by proc_lib:sync_start_link/2
eval_start_link(Pid, Timeout) ->
  receive
    {ack, Pid, Return} -> Return;
    {'EXIT', Pid, Reason} -> {error, Reason}
  after
    Timeout ->
      kill_pid(Pid),
      {error, timeout}
  end.


% Inspired by proc_lib:sync_start_monitor/2
eval_start_monitor(Pid, Ref, Timeout) ->
  receive
    {ack, Pid, Return} -> {Return, Ref};

    {'DOWN', Ref, process, Pid, Reason} = Down ->
      self() ! Down,
      {{error, Reason}, Ref}
  after
    Timeout ->
      kill_pid(Pid),
      {{error, timeout}, Ref}
  end.


% CONTINUE HERE
% - Add handling of anno_opts
% - Check associated by module
% - Check the load of the node
% - Select most suitable
% Functionality corresponding to the one in srdlib proc_lib.erl
proc_start(M, F, A, Timeout, Spawn_opts, _Anno_opts)
when is_atom(M), is_atom(F), is_list(A), is_integer(Timeout), is_list(Spawn_opts) ->
  ensure_no_monitor(M, F, A, Timeout, Spawn_opts),
  {Pid, Ref} = proc_lib:spawn_opt(M, F, A, [monitor | Spawn_opts]),
  eval_start(Pid, Ref, Timeout).


proc_start_link(M, F, A, Timeout, Spawn_opts, _Anno_opts)
when is_atom(M), is_atom(F), is_list(A), is_integer(Timeout), is_list(Spawn_opts) ->
  ensure_no_monitor(M, F, A, Timeout, Spawn_opts),
  {Pid, _Ref} = proc_lib:spawn_opt(M, F, A, [link | Spawn_opts]),
  eval_start_link(Pid, Timeout).


proc_start_monitor(M, F, A, Timeout, Spawn_opts, _Anno_opts)
when is_atom(M), is_atom(F), is_list(A), is_integer(Timeout), is_list(Spawn_opts) ->
  ensure_no_monitor(M, F, A, Timeout, Spawn_opts),
  {Pid, Ref} = proc_lib:spawn_opt(M, F, A, [monitor | Spawn_opts]),
  eval_start_monitor(Pid, Ref, Timeout).
