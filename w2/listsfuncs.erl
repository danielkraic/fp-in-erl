-module(listsfuncs).
-export([sum1/1, sum2/1, product1/1, product2/1, maximum1/1, maximum2/1]).

sum1([]) -> 0;
sum1([X|Xs]) ->  X + sum1(Xs).

sum2(Xs) -> sum2(Xs, 0).
sum2([], S) -> S;
sum2([X|Xs], S) -> sum2(Xs, X+S).


product1([]) -> 1;
product1([X|Xs]) -> X * product1(Xs).

product2([]) -> 0;
product2([X,Xs]) -> productXs(X,Xs).
productXs(S, []) -> S;
productXs(S, [X|Xs]) -> productXs(S+X, Xs).


maximum1([X]) -> X;
maximum1([X|Xs]) ->
    MAX = maximum1(Xs),
    case X > MAX of
        true -> X;
        false -> MAX
    end.

maximum2([X]) -> X;
maximum2([X|Xs]) -> maximumXs(X,Xs).
maximumXs(MAX, []) -> MAX;
maximumXs(MAX,[X|Xs]) ->
    case X > MAX of
        true -> maximumXs(X, Xs);
        _ -> maximumXs(MAX, Xs)
    end.