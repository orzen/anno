%%%-------------------------------------------------------------------
%% @doc anno public API
%% @end
%%%-------------------------------------------------------------------

-module(anno_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    anno_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
