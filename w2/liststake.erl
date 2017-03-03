-module(liststake).
-export([take1/2,take2/2,take3/2,take4/2]).

% -spec take(integer(), [T]) -> [T].

take1(N, _Xs) when N < 1
    -> [];
take1(N, [X|Xs]) when N > 0
    -> takeN(N, [X], Xs).

takeN(N,O,_Xs) when length(O) >= N
    -> lists:reverse(O, []);
takeN(_N,O,[])
    -> lists:reverse(O, []);
takeN(N,O,[X|Xs])
    -> takeN(N,[X|O], Xs).


take2(0, _Xs) ->
    [];
take2(_N, []) ->
    [];
take2(N, [X|Xs]) when N>0 ->
    [X|take2(N-1,Xs)].


take3(N,Xs) ->
    {Front,_Back} = lists:split(N,Xs),
    Front.

take4(N, List) ->
    take4(N, List, []).

take4(0, _L, R) ->
    lists:reverse(R, []);
take4(N, [H|T], R) ->
    take4(N-1, T, [H|R]);
take4(_, [], R) ->
    lists:reverse(R, []).