-module(nonl).

-export([name/0]).
-export([value/0]).
-export([taxed_value/0]).
-export([in_ca/0]).

name() ->
  "Tom".

value() ->
  10000.

taxed_value() ->
  float_to_list(value() - (value() * 0.4), [{decimals, 2}]).

in_ca() ->
  true.
