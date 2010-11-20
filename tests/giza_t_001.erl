-module(giza_t_001).

-include("giza.hrl").
-include("giza_internal.hrl").

-export([start/0]).

start() ->
  etap:plan(9),
  Query = giza_query:new(),
  etap:is("localhost", Query#giza_query.host, "Query defaults to localhost"),
  etap:is(3312, Query#giza_query.port, "Query port defaults to 3312"),
  etap:is(?SPHINX_MATCH_ALL, Query#giza_query.mode, "Query defaults to match all"),
  etap:is(?SPHINX_SORT_RELEVANCE, Query#giza_query.sort, "Query defaults to relevance sort"),
  etap:is(?SPHINX_GROUPBY_DAY, Query#giza_query.group_fun, "Query defaults to grouping by day"),
  etap:is(?SPHINX_GROUP_SORT_DESC, Query#giza_query.group_sort, "Query defaults to descending sort for groups"),
  etap:is(?SPHINX_COMMAND_SEARCH, Query#giza_query.command, "Query defaults to search"),
  etap:is(?SPHINX_COMMAND_SEARCH_VER, Query#giza_query.command_version, "Query defaults to correct search command version"),
  NewQuery = giza_query:query_string(Query, "foo"),
  etap:is(<<"foo">>, giza_query:query_string(NewQuery), "Query string gets turned into equivalent binary"),
  etap:end_tests().
