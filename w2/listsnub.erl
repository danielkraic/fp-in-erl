-module(listsnub).
-export([nub/1, bun/1]).

nub(List) ->
    nub(List, []).

nub([], Out) ->
    lists:reverse(Out, []);
nub([X|Xs], Out) ->
    case member(X,Out) of
        true -> nub(Xs, Out);
        false -> nub(Xs, [X|Out])
    end.


bun([]) ->
    [];
bun([X|Xs]) ->
    case member(X,Xs) of
        true -> bun(Xs);
        false -> [X|bun(Xs)]
    end.

member(_, []) ->
    false;
member(_X, [_X|_Xs]) ->
    true;
member(X, [_Y|Xs]) ->
    member(X, Xs).
