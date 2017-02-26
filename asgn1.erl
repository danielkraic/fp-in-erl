-module(asgn1).
-export([perimeter/1, area/1, enclose/1, bits/1]).

% perimeter of shape

perimeter({square, {_X,_Y}, {H}}) ->
    4*H;

perimeter({rectangle, {_X,_Y}, {H,W}}) ->
    2*H+2*W;

perimeter({circle, {_X,_Y}, R}) ->
    2*math:pi()*R;

perimeter({triangle, {A,B,C}}) ->
    A+B+C.

% area of shape

area({square, {_X,_Y}, {H}}) ->
    H*H;

area({rectangle, {_X,_Y}, {H,W}}) ->
    H*W;

area({circle, {_X,_Y}, R}) ->
    math:pi()*R*R;

area({triangle, {A,B,C}}) ->
    % based on Heron's formula
    S=(A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

% get smallest enclosing rectangle of shape

enclose({square, {X,Y}, {H}}) ->
    {{X,Y}, {X+H,Y+H}};

enclose({rectagle, {X,Y}, {H,W}}) ->
    {{X,Y}, {X+W,Y+H}};

enclose({circle, {X,Y}, R}) ->
    {{X-R,Y-R}, {X+R,Y+R}}.

% bits

bits(N) ->
    {_,_,S} = bitsNum(N,1,0), % return sum of bits
    S.

% N - input number to explore
% C - tail recursion counter (values representing each bit: 0,1,2,4,8,16,...)
% S - sum of bits that was matched

% counter extend input number, return result
bitsNum(N,C,S) when N < C ->
    {N,C,S};
% counter value bit matched in input number, increase bits sum and counter
bitsNum(N,C,S) when N band C == C ->
    bitsNum(N,C*2,S+1);
% counter value bit NOT matched in input number, increase counter and continue
bitsNum(N,C,S) ->
    bitsNum(N,C*2,S).

%% alternative:
% bits(V) ->
%     bits(V, 0).
% bits(0, C) ->
%     C;
% bits(V, C) ->
% bits( (V band (V - 1)), (C + 1)).
