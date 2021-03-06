-module(bench).
-export([run/0]).

-define(COUNT, 500).

run() ->
  Ctx0 = dict:from_list([{header, "Chris"}, {empty, false}, {list, true}]),
  A = dict:from_list([{name, "red"}, {current, true}, {url, "#Red"}]),
  B = dict:from_list([{name, "green"}, {current, false}, {url, "#Green"}]),
  C = dict:from_list([{name, "blue"}, {current, false}, {url, "#Blue"}]),
  Ctx1 = dict:store(item, [A, B, C], Ctx0),
  % Ctx1 = dict:new(),
  CT = mustache:compile(complex, "examples/complex.mustache"),
  T0 = erlang:monotonic_time(),
  render(CT, dict:to_list(Ctx1), ?COUNT),
  T1 = erlang:monotonic_time(),
  Diff = erlang:convert_time_unit(T1 - T0, native, microsecond),
  Mean = Diff / ?COUNT,
  io:format("Total time: ~.2fs~n", [Diff / erlang:convert_time_unit(1, millisecond, native)]),
  io:format("Mean render time: ~.2fms~n", [Mean / erlang:convert_time_unit(1, microsecond, native)]).

render(_CT, _Ctx, 0) ->
  ok;
render(CT, Ctx, N) ->
  Out = mustache:render(complex, CT, Ctx),
  % io:format(Out, []),
  100 = length(Out),
  % io:format(".", []),
  render(CT, Ctx, N - 1).
