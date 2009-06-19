-module(giza_util).


-export([index/2]).

index(List, Member) ->
  index(List, Member, 1).

%% Internal functions
index([], _, _) ->
  -1;
index([Member|_], Member, Count) ->
  Count;
index([_|T], Member, Count) ->
  index(T, Member, Count + 1).
