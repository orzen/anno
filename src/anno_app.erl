%%%-------------------------------------------------------------------
%% @doc anno public API
%% @end
%%%-------------------------------------------------------------------

-module(anno_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").
-include("modules.hrl").

start() -> startup().

start(_StartType, _StartArgs) -> startup().

stop(_State) ->
    anno_http:stop(),
    ok.

%% internal functions

startup() ->
    logger:set_primary_config(level, info),

    Nodes = case application:get_env(anno, nodes) of
        {ok, Val} -> Val;
        undefined -> []
    end,
    ?LOG_INFO("~p: ~p: cfg nodes '~p'", [?MODULE, ?FUNCTION_NAME, Nodes]),

    Modules = case application:get_env(anno, modules) of
        {ok, Val2} -> Val2;
        undefined -> []
    end,
    ?LOG_INFO("~p: ~p: cfg modules '~p'", [?MODULE, ?FUNCTION_NAME, Modules]),

    Sup = anno_sup:start_link(Nodes, Modules),
    %anno_http:start(),
    ?LOG_INFO("~p: ~p: completed", [?MODULE, ?FUNCTION_NAME]),
    Sup.
