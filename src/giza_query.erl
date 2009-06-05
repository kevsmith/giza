-module(giza_query).

-include("giza.hrl").
-include("giza_internal.hrl").

-define(COMMAND_VERSIONS, [{?SPHINX_COMMAND_SEARCH, ?SPHINX_COMMAND_SEARCH_VER},
                           {?SPHINX_COMMAND_EXCERPT, ?SPHINX_COMMAND_EXCERPT_VER},
                           {?SPHINX_COMMAND_UPDATE, ?SPHINX_COMMAND_UPDATE_VER},
                           {?SPHINX_COMMAND_KEYWORDS, ?SPHINX_COMMAND_KEYWORDS_VER}]).

-export([new/0, new/1, new/2]).
-export([query_string/1, query_string/2, command/1, command/2]).
-export([index/1, index/2]).

new() ->
  new_with_defaults().

new(Host, Port) when is_list(Host),
                     is_integer(Port) ->
  R = new_with_defaults(),
  R#giza_query{host=Host, port=Port};
new(Index, QueryString) when is_list(Index),
                             is_list(QueryString) ->
  R = new_with_defaults(),
  index(query_string(R, QueryString), Index).

new(QueryString) when is_list(QueryString) ->
  R = new_with_defaults(),
  query_string(R, QueryString).

query_string(Query) ->
  Query#giza_query.query_string.

query_string(Query, NewQueryString) when is_list(NewQueryString) ->
  query_string(Query, list_to_binary(NewQueryString));
query_string(Query, NewQueryString) when is_binary(NewQueryString) ->
  set_query_field(query_string, Query, NewQueryString).

index(Query) ->
  Query#giza_query.index.

index(Query, Index) when is_list(Index) ->
  index(Query, list_to_binary(Index));
index(Query, Index) when is_binary(Index) ->
  Query#giza_query{index=Index}.

command(Query) ->
  Query#giza_query.command.

command(Query, Command) when is_integer(Command) ->
  set_query_field(command, Query, Command).

%%Internal functions
new_with_defaults() ->
  set_query_field(command, #giza_query{mode=?SPHINX_MATCH_ALL,
                                       sort=?SPHINX_SORT_RELEVANCE,
                                       group_fun=?SPHINX_GROUPBY_DAY,
                                       group_sort=?SPHINX_GROUP_SORT_DESC,
                                       ranker=?SPHINX_RANK_PROXIMITY_BM25},
                  ?SPHINX_COMMAND_SEARCH).

set_query_field(mode, Query, MatchMode) ->
  Query#giza_query{mode=MatchMode};
set_query_field(sort_by, Query, SortBy) ->
  Query#giza_query{sort_by=SortBy};
set_query_field(query_string, Query, QueryString) ->
  Query#giza_query{query_string=QueryString};
set_query_field(command, Query, Command) when Command >= ?SPHINX_COMMAND_SEARCH andalso
                                              Command =< ?SPHINX_COMMAND_KEYWORDS ->
 case proplists:get_value(Command, ?COMMAND_VERSIONS) of
   undefined ->
     throw({error, {unknown_command, Command}});
   Version ->
     Query#giza_query{command=Command,
                      command_version=Version}
 end.
