-module(hofs).
-export([add/1,times/1, compose/2, id/1, iterate/1, compose/1, twice/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) -> X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

iterate(0) ->
    fun(Fn) -> Fn end;
iterate(N) ->
    fun(F) -> 
        FList = lists:duplicate(N, F),
        compose(FList)
    end.


compose(Fs) ->
    fun(X) -> lists:foldr(fun(F, Res) -> F(Res) end, X, Fs) end.

twice(F) ->
    compose([F,F]).