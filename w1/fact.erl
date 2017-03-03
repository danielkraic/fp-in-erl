-module(fact).
-export([fact/1]).

fact(0) ->
    1;
fact(N) when N>0 ->
    fact(N-1)*N.

% tail recursion
fac(N) ->
  fac(N,1).

fac(0, P) ->
  P;
fac(N,P) when N>0 ->
  fac(N-1,P*N).