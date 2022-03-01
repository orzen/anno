-module(gen_server_S).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1]).

% Test cases
-export([start_gen_server/1]).

suite() -> [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
  ok = application:start(anno),
  Config.


end_per_suite(_Config) -> application:stop(anno).

groups() -> [].

all() -> [start_gen_server].

start_gen_server(Config) ->
  ct:print("start TEST CASE '~p'", [Config]),
  {ok, Pid} = gen_proc:start_link(somename),
  All = ets:foldl(fun (Entry, Acc) -> [Entry | Acc] end, [], anno_proc),
  ct:print("SOMENAME '~p', ALL '~p'", [Pid, All]),
  Pid2 = anno:whereis_name(somename),
  ct:print("WHEREIS '~p'", [Pid2]),
  Res = gen_proc:stop(somename),
  All = ets:foldl(fun (Entry, Acc) -> [Entry | Acc] end, [], anno_proc),
  ct:print("RES '~p', ALL '~p'", [Res, All]),
  ok.
