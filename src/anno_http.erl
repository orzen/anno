-module(anno_http).

-export([start/0, stop/0]).

start() ->
  Dispatch =
    cowboy_router:compile(
      [{"_", [{"/", anno_api, [<<"anno v0.0.1">>]}, {"/node/[:node_id]", anno_api_node, []}]}]
    ),
  {ok, _} = cowboy:start_clear(http, [{port, 7878}], #{env => #{dispatch => Dispatch}}).


stop() -> ok = cowboy:stop_listener(http).
