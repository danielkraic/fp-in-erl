-module(perfect).
-export([per/1]).

per(N) when N > 0 -> perf(N,N-1,0).

perf(N,0,S) when N == S -> true;
perf(_,0,_) -> false;
perf(N,C,S) when N rem C == 0 -> perf(N,C-1,S+C);
perf(N,C,S) -> perf(N, C-1, S).