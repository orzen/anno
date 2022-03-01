-module(anno_modules).

% API exports
-export([associate_modules/1, lookup/1, start_link/1, stop/0]).

% Callback exports
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-include("gen_server_log.hrl").

-define(SERVER, {global, ?MODULE}).
-define(TAB, anno_modules).

-record(state, {tid}).
-record(module, {id :: atom(), labels = [] :: list()}).

%% API

associate_modules(Modules) when is_list(Modules) ->
  gen_server:cast(?SERVER, {associate_modules, Modules}).

lookup(Module) -> gen_server:call(?SERVER, {lookup_module, Module}).

start_link(Modules) -> gen_server:start_link(?SERVER, ?MODULE, [Modules], []).

stop() -> gen_server:stop(?SERVER).

%% CALLBACK

% Modules = [{some_module_name, [
%  {labels, [labelx, labely]}
% ]}]
init([Modules]) when is_list(Modules) ->
  ?GS_INIT(Modules),
  T_modules = ets:new(anno_modules, [named_table, bag]),
  associate_modules1(Modules),
  {ok, #state{tid = T_modules}}.


terminate(Reason, _State) ->
  ?GS_TERM(Reason),
  ok.


handle_call({lookup_module, Module}, _From, State) ->
  {ok, Labels} = lookup_module(Module),
  Resp = {ok, Module, Labels},
  {reply, Resp, State, hibernate};

handle_call(Req, From, State) ->
  ?GS_UNHANDLED_CALL(Req, From),
  {noreply, State, hibernate}.


handle_cast({associate_modules, Modules}, State) ->
  % This should not be able to fail
  associate_modules1(Modules),
  {noreply, State, hibernate};

handle_cast(Req, State) ->
  ?GS_UNHANDLED_CAST(Req),
  {noreply, State, hibernate}.


handle_info(Req, State) ->
  ?GS_UNHANDLED_INFO(Req),
  {noreply, State, hibernate}.

%% internal functions

lookup_module(Module) when is_atom(Module) ->
  case ets:match(?TAB, {Module, '$1'}) of
    [] -> {error, undefined};
    [[Labels]] -> {ok, Labels}
  end.


associate_modules1(Modules) ->
  F = fun (#module{id = ID, labels = Labels}, Acc) -> [{ID, Labels} | Acc] end,
  Modules1 = lists:foldl(F, [], Modules),
  true = ets:insert(?TAB, Modules1).
