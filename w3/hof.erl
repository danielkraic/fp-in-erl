-module(hof).
-export([map/2, filter/2, reduce/3, doubleAll/1, evens/1, product/1, zip/2, zip_with/3, zip_map/2, zip_map_with/3]).

map(_F, []) ->
    [];
map(F, [X|Xs]) ->
    [F(X) | map(F, Xs)].


filter(_P, []) ->
    [];
filter(P, [X|Xs]) ->
    case P(X) of
        true -> [X | filter(P, Xs)];
        false -> filter(P, Xs)
    end.


reduce(_Combine, Start, []) ->
    Start;
reduce(Combine, Start, [X|Xs]) ->
    Combine(X, reduce(Combine, Start, Xs)).


doubleAll(Xs) ->
    hof:map(fun(X) -> X*X end, Xs).


evens(Xs) ->
    hof:filter(fun(X) -> X rem 2 == 0 end, Xs).


product(Xs) ->
    hof:reduce(fun(X,Y) -> X*Y end, 1, Xs).


zip(Xs, Ys) ->
    Res = zip(Xs, Ys, []),
    lists:reverse(Res).

zip([], _Ys, Res) ->
    Res;
zip(_Xs, [], Res) ->
    Res;
zip([X|Xs], [Y|Ys], Res) ->
    zip(Xs, Ys, [{X,Y} | Res]).


zip_with(F, Xs, Ys) ->
    Res = zip_with(F, Xs, Ys, []),
    lists:reverse(Res).

zip_with(_F, [], _Ys, Res) ->
    Res;
zip_with(_F, _Xs, [], Res) ->
    Res;
zip_with(F, [X|Xs], [Y|Ys], Res) ->
    zip_with(F, Xs, Ys, [F(X,Y) | Res]).


zip_map(Xs, Ys) ->
    map(fun({X,Y}) -> {X,Y} end, zip(Xs, Ys)).

zip_map_with(F, Xs, Ys) ->
    map(F, zip(Xs, Ys)).