-module(xOr).
-export([xOr/2, xOr2/2, xOr3/2, xOr4/2, maxThree/3, howManyEqual/3]).

xOr(true, false) ->
    true;
xOr(false, true) ->
    true;
xOr(_,_) ->
    false.

xOr1(true,X) ->
    not(X);
xOr1(false,X) ->
    X.

xOr2(X,X) ->
    false;
xOr2(_,_) ->
    true.

xOr3(A,B) when A =/= B ->
    true;
xOr3(_,_) ->
    false.

xOr4(A,B) when A == B ->
    false;
xOr4(_,_) ->
    true.

maxThree(A,B,C) when A > B, B > C ->
    A;
maxThree(_,B,C) when B > C ->
    B;
maxThree(_, _, C) ->
    C.

howManyEqual(X,X,X) ->
    3;
howManyEqual(X,X,_) ->
    2;
howManyEqual(X,_,X) ->
    2;
howManyEqual(_,X,X) ->
    2;
howManyEqual(_,_,_) ->
    0.
