-module(fibonacci).
-export([fib/1, fib2/1, fib3/1]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N>0 ->
    fib(N-2) + fib(N-1).

% tail recursion
fib2(N) ->
    fib(N, 1, 0).

fib(0,_,B) ->
    B;
fib(N,A,B) when N>0 ->
    fib(N-1, A+B, A).


% tail recursion iwith pattern matching
fib3(N) when N >= 0 ->
    {P, _} = fibP(N),
    P.

fibP(0) ->
    {0, 1};
fibP(N) ->
    {P,C} = fibP(N-1),
    {C,P+C}.