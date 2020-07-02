-module(unescaped).

-export([title/0]).
-export([start/0]).

title() ->
  "Bear > Shark".

%%---------------------------------------------------------------------------

start() ->
  code:add_patha(".."),
  Output = mustache:render(unescaped, "unescaped.mustache"),
  io:format(Output, []).
