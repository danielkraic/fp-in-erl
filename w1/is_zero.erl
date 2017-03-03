-module(is_zero).
-export([is_zero/1]).

is_zero(0) ->
    true;
is_zero(_) ->
    false.