-module(nonl).

-export([name/0]).
-export([value/0]).
-export([taxed_value/0]).
-export([in_ca/0]).

name() ->
  "Tom".

value() ->
  "10000".

taxed_value() ->
  integer_to_list(value() - (value() * 0.4)).

in_ca() ->
  true.
