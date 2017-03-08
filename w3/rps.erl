-module(rps).
-export([beats/1, loose/1, result/2, tournament/2]).

beats(rock) -> paper;
beats(paper) -> scissors;
beats(scissors) -> rock.

loose(rock) -> scissors;
loose(scissors) -> paper;
loose(paper) -> rock.

result(_X,_X) ->
    0;
result(X,Y) ->
    case loose(X) == Y of
        true -> 1;
        false -> -1
    end.

tournament(Left, Right) ->
    Rounds = lists:zip(Left, Right),
    Results = lists:map(fun({L,R}) -> result(L,R) end, Rounds),
    lists:sum(fun(Result, Sum) -> Result + Sum end, 0, Results).