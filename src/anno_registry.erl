-module(anno_registry).

-behavior(gen_server).

% Extended API
-export([apply_labels/2]).

% Mandatory API for a process registry
% Listed here https://erlang.org/doc/man/gen_server.html#start_link-4
-export([register_name/2,
         unregister_name/1,
         whereis_name/1,
         send/2]).
-export([start_link/0,
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/include/logger.hrl").
-include("gen_server_log.hrl").

-define(SERVER, {global, ?MODULE}).

-record(state, {}).
-record(proc, {id, pid, node, labels}).

% Apply labels to a registered name.
-spec apply_labels(Name :: term(), Labels :: list()) -> ok | undefined.
apply_labels(_Name, _Labels) ->
  %Each = fun()
  ok.

%% Anno server: client API

-spec register_name(Name :: term(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) -> gen_server:call(?SERVER, {register_name, Name, Pid}).

-spec unregister_name(Name :: term()) -> term().
unregister_name(Name) ->
  gen_server:cast(?SERVER, {unregister_name, Name}),
  Name.


-spec send(Name :: term(), Msg :: term()) -> ok.
send(Name, Msg) -> gen_server:call(?SERVER, {send, Name, Msg}).

-spec whereis_name(Name :: term()) -> pid() | undefined.
whereis_name(Name) -> gen_server:call(?SERVER, {whereis_name, Name}).

%% Gen server client API

start_link() ->
  gen_server:start_link(?SERVER, ?MODULE, [], []).

%% Gen server callbacks

init(_Args) ->
  ?GS_INIT([]),
  ets:new(anno_proc, [named_table, bag]),
  {ok, #state{}, hibernate}.


terminate(Reason, _State) ->
  ?LOG_DEBUG("~p: ~p: terminating '~p'", [?MODULE, ?FUNCTION_NAME, Reason]),
  normal.

%% TODO check ets:match_object(Tab, #proc{name=Name, _=''})

% Call
handle_call({send, Name, Msg}, _From, State) ->
  ?LOG_DEBUG("~p: ~p: send '~p' '~p'", [?MODULE, ?FUNCTION_NAME, Name, Msg]),
  Reply =
    case ets:lookup(anno_proc, Name) of
      [] -> {badarg, {Name, Msg}};

      [#proc{pid = Pid}] ->
        Pid ! Msg,
        Pid
    end,
  Name ! Msg,
  {reply, Reply, State, hibernate};

handle_call({register_name, Name, Pid}, _From, State) ->
  Reply =
    case ets:lookup(anno_proc, Name) of
      [] ->
        Proc = #proc{id = Name, pid = Pid, labels = []},
        true = ets:insert(anno_proc, Proc),
        yes;

      [_Node] -> no
    end,
  ?LOG_DEBUG("~p: ~p: register_name '~p' '~p'", [?MODULE, ?FUNCTION_NAME, Name, Reply]),
  {reply, Reply, State, hibernate};

handle_call({whereis_name, Name}, From, State) ->
  ?LOG_DEBUG("~p: ~p: whereis_name '~p', from '~p'", [?MODULE, ?FUNCTION_NAME, Name, From]),
  Reply =
    case ets:match(anno_proc, #proc{id = Name, pid = '$1', node = '_', labels = '_'}) of
      [] -> undefined;

      [[Pid]] ->
        ?LOG_NOTICE("~p:~p: whereis success pid '~p'", [?MODULE, ?FUNCTION_NAME, Pid]),
        Pid;

      Other ->
        ?LOG_WARNING("~p:~p: unmatched '~p'", [?MODULE, ?FUNCTION_NAME, Other]),
        Other
    end,
  ?LOG_DEBUG("~p: ~p: whereis_name '~p' '~p'", [?MODULE, ?FUNCTION_NAME, Name, Reply]),
  {reply, Reply, State, hibernate};

handle_call(Req, _From, State) ->
  ?LOG_DEBUG("~p: ~p: handle_call '~p'", [?MODULE, ?FUNCTION_NAME, Req]),
  {noreply, State, hibernate}.


% Cast
handle_cast({unregister_name, Name}, State) ->
  ?LOG_DEBUG("~p: ~p: unregister_name '~p'", [?MODULE, ?FUNCTION_NAME, Name]),
  true = ets:match_delete(anno_proc, #proc{id = Name, pid = '_', node = '_', labels = '_'}),
  {noreply, State, hibernate};

handle_cast(Req, State) ->
  ?LOG_DEBUG("~p: ~p: handle_cast '~p'", [?MODULE, ?FUNCTION_NAME, Req]),
  {noreply, State, hibernate}.


% Info
handle_info(Msg, State) ->
  ?LOG_DEBUG("~p: ~p: handle_info '~p'", [?MODULE, ?FUNCTION_NAME, Msg]),
  {noreply, State, hibernate}.
