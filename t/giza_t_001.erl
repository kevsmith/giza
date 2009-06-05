-module(giza_t_001).

-include("giza_internal.hrl").
-include("giza.hrl").

-export([start/0]).

start() ->
  etap:plan(9),
  Query = giza_query:new(),
  etap:ok(Query#giza_query.host =:= "localhost", "Query defaults to localhost"),
  etap:ok(Query#giza_query.port == 3312, "Query port defaults to 3312"),
  etap:ok(Query#giza_query.mode == ?SPHINX_MATCH_ALL, "Query defaults to match all"),
  etap:ok(Query#giza_query.sort == ?SPHINX_SORT_RELEVANCE, "Query defaults to relevance sort"),
  etap:ok(Query#giza_query.group_fun == ?SPHINX_GROUPBY_DAY, "Query defaults to grouping by day"),
  etap:ok(Query#giza_query.group_sort =:= ?SPHINX_GROUP_SORT_DESC, "Query defaults to descending sort for groups"),
  etap:ok(Query#giza_query.command =:= ?SPHINX_COMMAND_SEARCH, "Query defaults to search"),
  etap:ok(Query#giza_query.command_version =:= ?SPHINX_COMMAND_SEARCH_VER, "Query defaults to correct search command version"),
  NewQuery = giza_query:query_string(Query, "foo"),
  etap:ok(giza_query:query_string(NewQuery) =:= <<"foo">>, "Query string gets turned into equivalent binary"),
  etap:end_tests().
