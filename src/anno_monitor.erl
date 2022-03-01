-module(anno_monitor).

-behaviour(gen_server).

-export([monitor/1, demonitor/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, {global, ?MODULE}).

monitor(Name) -> gen_server:call(?SERVER, {monitor, self(), Name}).

demonitor(MRef) -> gen_server:call(?SERVER, {demonitor, MRef}).

init(_Args) ->
  Ets = ets:new(anno_monitor, [bag, named_table]),
  {ok, Ets, hibernate}.


terminate(_Reason, _State) -> normal.

handle_call({monitor, Observer, Name}, _From, State) ->
  Pid = anno_registry:whereis_name(Name),
  MRef = monitor(process, Name),
  true = ets:insert(State, {MRef, Observer, Pid, Name}),
  {reply, MRef, State, hibernate};

handle_call({demonitor, MRef}, _From, State) ->
  Reply = ets:delete(State, MRef),
  {noreply, Reply, State, hibernate};

handle_call(Req, From, State) ->
  ?LOG_DEBUG("~p: ~p: unhandled call '~p' '~p'", [?MODULE, ?FUNCTION_NAME, From, Req]),
  {noreply, State, hibernate}.


handle_cast(Req, State) ->
  ?LOG_DEBUG("~p: ~p: unhandled cast '~p'", [?MODULE, ?FUNCTION_NAME, Req]),
  {noreply, State, hibernate}.


handle_info({'DOWN', MonitorRef, Type, Object, Info}, State) ->
  [{_, Observer_pid, _, _}] = ets:take(State, MonitorRef),
  Observer_pid ! {'DOWN', MonitorRef, Type, Object, Info},
  {noreply, State, hibernate};

handle_info(Msg, State) ->
  ?LOG_DEBUG("~p: ~p: unhandled info '~p'", [?MODULE, ?FUNCTION_NAME, Msg]),
  {noreply, State, hibernate}.
