-module(anno).

-export([register_name/2,
         unregister_name/1,
         whereis_name/1]).

-include_lib("kernel/include/logger.hrl").

% Registry
register_name(Name, Pid) ->
  ?LOG_NOTICE("CALLED ~p:~p", [?MODULE, ?FUNCTION_NAME]),
  anno_registry:register_name(Name, Pid).


unregister_name(Name) ->
  ?LOG_NOTICE("CALLED ~p:~p", [?MODULE, ?FUNCTION_NAME]),
  anno_registry:unregister_name(Name).


whereis_name(Name) ->
  ?LOG_NOTICE("CALLED ~p:~p", [?MODULE, ?FUNCTION_NAME]),
  anno_registry:whereis_name(Name).
