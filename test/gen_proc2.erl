-module(gen_proc2).

-behaviour(gen_server).

-export([call/2, cast/2, send/2, start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(VIA(X), {via, anno, X}).

call(Name, Req) -> gen_server:call(?VIA(Name), Req).

cast(Name, Req) -> gen_server:cast(?VIA(Name), Req).

send(Name, Msg) -> ?VIA(Name) ! Msg.

start_link(Name) -> gen_server:start_link(?VIA(Name), ?MODULE, [], []).

stop(Name) -> gen_server:stop(?VIA(Name)).

init(_Args) ->
  ?LOG_INFO("~p[~p]: init", ?MODULE, self()),
  {ok, [], hibernate}.


terminate(_Reason, _State) -> normal.

handle_call(_Req, _From, State) ->
  ?LOG_INFO("~p[~p]: call", ?MODULE, self()),
  {noreply, State, hibernate}.


handle_cast(_Req, State) ->
  ?LOG_INFO("~p[~p]: cast", ?MODULE, self()),
  {noreply, State, hibernate}.


handle_info(_Req, State) ->
  ?LOG_INFO("~p[~p]: info", ?MODULE, self()),
  {noreply, State, hibernate}.
