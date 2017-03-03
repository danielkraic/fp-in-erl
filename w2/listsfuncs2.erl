-module(listsfuncs).
-export([]).

total_area(shapes) ->
    sum(all_areas(shaped)).

% map
all_areas([]) -> [];
all_areas([X,Xs]) -> [area(X) | all_areas(Xs)].