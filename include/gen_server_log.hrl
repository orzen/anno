-define(GS_INIT(X), ?LOG_INFO("~p: ~p: args '~w'", [?MODULE, ?FUNCTION_NAME, X])). % TODO change to debug
-define(GS_TERM(X), ?LOG_INFO("~p: ~p: reason '~w'", [?MODULE, ?FUNCTION_NAME, X])).
-define(
  GS_UNHANDLED_CALL(REQ, FROM),
  ?LOG_WARNING("~p: ~p: unhandled call '~w' from '~w'", [?MODULE, ?FUNCTION_NAME, REQ, FROM])
).
-define(
  GS_UNHANDLED_CAST(REQ),
  ?LOG_WARNING("~p: ~p: unhandled cast '~w'", [?MODULE, ?FUNCTION_NAME, REQ])
).
-define(
  GS_UNHANDLED_INFO(REQ),
  ?LOG_WARNING("~p: ~p: unhandled message '~w'", [?MODULE, ?FUNCTION_NAME, REQ])
).
