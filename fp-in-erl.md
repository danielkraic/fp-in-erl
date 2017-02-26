# erlang

```bash
erl # run erlang shell
```

```erlang
q(). % quit shell
```

```erlang
-module(first).
-export([double/1, mult/2, area/3, square/1, tremble/1]).

mult(X,Y) ->
X*Y.

double(X) ->
mult(2,X).

area(A,B,C) ->
S = (A+B+C)/2,
math:sqrt(S*(S-A)*(S-B)*(S-C)).

square(A) ->
A*A.

tremble(A) ->
A*A*A.
```

```erlang
c(first).
first:double(10).
```

```erlang
-module(second).
-export([hypotenuse/2, perimeter/2, area/2]).

hypotenuse(X, Y) ->
math:sqrt(first:square(X) + first:square(Y)).

perimeter(A,B) ->
A+B+hypotenuse(A,B).

area(A,B) ->
first:mult(A,B)/2.
```

## data types

* numbers
* atoms
* booleans
* lists and tuples
* strings
* functions

### numbers

* integers and floats
* big numbers
* full precision

```erlang
2#100. % base#number
```

```erlang
% usual operators: + - * / div rem
12 div 5.
```

### atoms

```erlang
foo.
'foot'.

foo == foo.
foo > 'I am atom'.
```

### booleans

* two special atoms

```erlang
true.
false.

not true.
true and false.
```

* not
* and
* or
* xor

```erlang
Expr1 orelse Expr2
Expr1 andalso Expr2
```

```erlang
false and (3 == (4 div 0)).     % exception
false andalso (3 == (4 div 0)). % false
true orelse (3 == (4 div 0)).   % true
```

### tuples

```erlang
{'joe', 'armstrong', 55}.
{1.2,5}.
{{1,2,3}, {4,5,6}}.
```

```erlang
% used as set of atributes
{rectangle, {1,2}, {3,5}}.
{circle, {1,2}, 3}.
```

### lists

```erlang
['Joe', 'Armstrong'].
[{1,2}].
[{1,2}, {4,4}].
[].
[[1,2,3]].
```

```erlang
length([1,2,3,4]).
```

```erlang
[1,2,3] ++ [4,5,6,7].
```

### strings

* list of characters
* $c - get ascii code for char c

```erlang
'abc'.      % "abc"
[97,98,99]. % "abc"
[a,b,c].    % [a,b,c]
[$a,$b,$c]. % "abc"

[$h,$e,$l,$l,$o]. % "hello"
[$h,$e,$l,$l,o].  % [104,101,108,108,o]

[34,3,5,36,37]. % [34,3,5,36,37]
[34,35,36,37].  % "\"#$%"
```

### functions

* functions can be data themselves

```erlang
fun (X) -> X*2 end.
```

* functions can be argumens of other functions

```erlang
lists:map(fun (X) -> X*3 end, [1,2,3]).

Fn1 = fun (X) -> X*2 end.
lists:map(Fn1, [1,2,3]).
```

```erlang
% foldr: reduce
lists:foldr(fun (X,Y) -> X*Y end, 1, [1,2,3,4,5,6]).
```

```erlang
(fun (X) -> X+2 end)(40). % 40
fun (X) -> X+2 end(40).
```

## variables

* single assignment
* variables are names to values
* if value is already bound, then '=' is check on its value

```erlang
A=1+2.
B=A+2.
```

```erlang
{A,B}={2,3}.

{A,A} = {2,2}
```

```erlang
A=4.
f(A). % forget binding (only in shell)
A=6.

f(). % forget all bindings (only in shell)
```

## pattern matching

```erlang
is_zero(0) ->
    true;
is_zero(_) ->
    false.
```

```erlang
xOr(true,false) ->
  true;
xOr(false,true) ->
  true;
xOr(_,_) ->
  false.
```

```erlang
xOr(X,X) ->
  false;
xOr(_,_) ->
  true.
```

# recursion

## guards

* can be combined with and *,* and with or ';'

## direct recursion

```
fact(0) ->
    1;
fact(N) when N>0 ->
    fact(N-1)*N.
```

```erlang
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N>0 ->
    fib(N-2) + fib(N-1).
```

```erlang
pieces(0) ->
  1;
pieces(N) ->
  N + pieces(N-1).
```

## tail recursion

```erlang
fac(N) ->
  fac(N,1).

fac(0, P) ->
  P;
fac(N,P) when N>0 ->
  fac(N-1,P*N).
```

```erlang
loop(N) when N>0 ->
    io:format("~p~n", [N]),
    loop(N-1);
loop(_) ->
    io:format("bye~n").
```

```erlang
fib2(N) ->
    fib(N, 1, 0).

fib(0,_,B) ->
    B;
fib(N,A,B) when N>0 ->
    fib(N-1, A+B, A).
```

```erlang
per(N) when N > 0 -> perf(N,N-1,0).

perf(N,0,S) when N == S -> true;
perf(_,0,_) -> false;
perf(N,C,S) when N rem C == 0 -> perf(N,C-1,S+C);
perf(N,C,S) -> perf(N, C-1, S).
```