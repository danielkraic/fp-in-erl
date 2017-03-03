-module(listsfuncs3).
-export([double/1, evens/1]).

double([]) ->
    [];
double([X|Xs]) ->
    [X*X|double(Xs)].

evens([]) ->
    [];
evens([X|Xs]) ->
    case is_even(X) of
        true -> [X | evens(Xs)];
        false -> evens(Xs)
    end.

is_even(X) when X rem 2 == 0 ->
    true;
is_even(_) ->
    false.

