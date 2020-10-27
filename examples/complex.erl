-module(complex).

-export([header/0]).
-export([item/0]).
-export([link/1]).
-export([list/0]).
-export([empty/0]).
-export([start/0]).

header() ->
  "Colors".

item() ->
  A = dict:from_list([{name, "red"}, {current, true}, {url, "#Red"}]),
  B = dict:from_list([{name, "green"}, {current, false}, {url, "#Green"}]),
  C = dict:from_list([{name, "blue"}, {current, false}, {url, "#Blue"}]),
  [A, B, C].

link(Ctx) ->
  mustache:get(current, Ctx).

list() ->
  length(item()) =/= 0.

empty() ->
  length(item()) =:= 0.

%%---------------------------------------------------------------------------

start() ->
  _ = code:add_patha(".."),
  Output = mustache:render(complex, "complex.mustache"),
  io:format(Output, []).
