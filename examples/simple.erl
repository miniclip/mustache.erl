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
  code:add_patha(".."),
  Ctx = dict:from_list([{name, "TPW"}]),
  Output = mustache:render(simple, "simple.mustache", Ctx),
  io:format(Output, []).
