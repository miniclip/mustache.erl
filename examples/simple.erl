-module(simple).

-export([name/0]).
-export([value/0]).
-export([taxed_value/0]).
-export([in_ca/0]).
-export([start/0]).

name() ->
  "Tom".

value() ->
  10000.

taxed_value() ->
  value() - (value() * 0.4).

in_ca() ->
  true.

%%---------------------------------------------------------------------------

start() ->
  _ = code:add_patha(".."),
  Ctx = [{name, "TPW"}],
  Output = mustache:render(simple, "simple.mustache", Ctx),
  io:format(Output, []).
