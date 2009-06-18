%% Copyright (c) 2009 Electronic Arts, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(giza_query).

-author("Kevin A. Smith <kevin@hypotheticalabs.com>").

-include("giza.hrl").
-include("giza_internal.hrl").

-define(COMMAND_VERSIONS, [{?SPHINX_COMMAND_SEARCH, ?SPHINX_COMMAND_SEARCH_VER},
                           {?SPHINX_COMMAND_EXCERPT, ?SPHINX_COMMAND_EXCERPT_VER},
                           {?SPHINX_COMMAND_UPDATE, ?SPHINX_COMMAND_UPDATE_VER},
                           {?SPHINX_COMMAND_KEYWORDS, ?SPHINX_COMMAND_KEYWORDS_VER}]).

-export([new/0, new/1, new/2]).
-export([query_string/1, query_string/2]).
-export([host/1, host/2, port/1, port/2]).
-export([index/1, index/2, limit/1, limit/2]).
-export([offset/1, offset/2, min_id/1, min_id/2]).
-export([max_id/1, max_id/2]).
-export([filters/1, add_filter/3, remove_filter/2]).

%% @spec new() -> Result
%%       Result = any()
%% @doc Create a new giza query pointing with default values
new() ->
  new_with_defaults().

%% @spec new(Host, Port) -> Result
%%       Host = string()
%%       Port = integer()
%%       Result = any()
%% @doc Create a new giza query pointing to the given host and port
new(Host, Port) when is_list(Host),
                     is_integer(Port) ->
  R = new_with_defaults(),
  R#giza_query{host=Host, port=Port};

%% @spec new(Index, QueryString) -> Result
%%       Index = string()
%%       QueryString = string()
%%       Result = any()
%% @doc Create a new giza query using the specified index name and query string
new(Index, QueryString) when is_list(Index),
                             is_list(QueryString) ->
  R = new_with_defaults(),
  index(query_string(R, QueryString), Index).

%% @spec new(QueryString) -> Result
%%       QueryString = string()
%%       Result = any()
%% @doc Create a new giza query with the specified query string
new(QueryString) when is_list(QueryString) ->
  R = new_with_defaults(),
  query_string(R, QueryString).

%% @spec query_string(Query) -> Result
%%       Query = any()
%%       Result = binary()
%% @doc Retrieve the current query string
query_string(Query) ->
  Query#giza_query.query_string.

%% @spec query_string(Query, NewQueryString) -> Result
%%       Query = any()
%%       NewQueryString = string()
%%       Result = any()
%% @doc Set the query string
query_string(Query, NewQueryString) when is_list(NewQueryString) ->
  query_string(Query, list_to_binary(NewQueryString));
query_string(Query, NewQueryString) when is_binary(NewQueryString) ->
  set_query_field(query_string, Query, NewQueryString).

%% @spec index(Query) -> Result
%%       Query = any()
%%       Result = string()
%% @doc Get the current search index name
index(Query) ->
  Query#giza_query.index.

%% @spec index(Query, Index) -> Result
%%       Query = any()
%%       Index = string()
%%       Result = any()
%% @doc Set the search index name
index(Query, Index) when is_list(Index) ->
  index(Query, list_to_binary(Index));
index(Query, Index) when is_binary(Index) ->
  Query#giza_query{index=Index}.

%% @spec host(Query) -> Result
%%       Query = any()
%%       Result = string()
%% @doc Get the current target host
host(Query) ->
  Query#giza_query.host.

%% @spec host(Query, Host) -> Result
%%       Query = any()
%%       Host = string()
%%       Result = any()
%% @doc Set the target host
host(Query, Host) when is_list(Host) ->
  set_query_field(host, Query, Host).

%% @spec port(Query) -> Result
%%       Query = any()
%%       Result = integer()
%% @doc Get the current target port
port(Query) ->
  Query#giza_query.port.

%% @spec port(Query, Port) -> Result
%%       Query = any()
%%       Port = integer()
%%       Result = any()
%% @doc Set the current target port
port(Query, Port) when is_number(Port) ->
  set_query_field(port, Query, Port).

%% @spec limit(Query) -> Result
%%       Query = any()
%%       Result = integer()
%% @doc Get the current query result limit
limit(Query) ->
  Query#giza_query.limit.

%% @spec limit(Query, Limit) -> Result
%%       Query = any()
%%       Limit = integer()
%%       Result = any()
%% @doc Set the query result limit
limit(Query, Limit) when is_number(Limit) ->
  set_query_field(limit, Query, Limit).

%% @spec offset(Query) -> Result
%%       Query = any()
%%       Result = integer()
%% @doc Get the current offset
offset(Query) ->
  Query#giza_query.offset.

%% @spec offset(Query, Offset) -> Result
%%       Query = any()
%%       Offset = integer()
%%       Result = any()
%% @doc Set the query offset
offset(Query, Offset) ->
  set_query_field(offset, Query, Offset).

%% @spec min_id(Query) -> Result
%%       Query = any()
%%       Result = integer()
%% @doc Get the minimum doc id limit
min_id(Query) ->
  Query#giza_query.min_id.

%% @spec min_id(Query, MinId) -> Result
%%       Query = any()
%%       MinId = integer()
%%       Result = any()
%% @doc Set the minimum doc id limit
min_id(Query, MinId) ->
  set_query_field(min_id, Query, MinId).

%% @spec max_id(Query) -> Result
%%       Query = any()
%%       Result = integer()
%% @doc Get the maximum doc id limit
max_id(Query) ->
  Query#giza_query.max_id.

%% @spec max_id(Query, MaxId) -> Result
%%       Query = any()
%%       MinId = integer()
%%       Result = any()
%% @doc Set the maximum doc id limit
max_id(Query, MaxId) ->
  set_query_field(max_id, Query, MaxId).

%% @spec filters(Query) -> Result
%%       Query = any()
%%       Result = list(tuple())
%% @doc Retrieve currently set filters
filters(Query) ->
  Query#giza_query.filters.

%% @spec add_filter(Query, Name, Values) -> Result
%%       Query = any()
%%       Name = list()
%%       Values = list() | binary()
%%       Result = any()
%% @doc Add a new filter to a query
add_filter(Query, Name, Values) when is_list(Name) ->
  add_filter(Query, list_to_binary(Name), Values);
add_filter(#giza_query{filters=Filters}=Query, Name, Values) when is_binary(Name) ->
  Query#giza_query{filters=[{Name, Values}|Filters]}.

%% @spec remove_filter(Query, Name) -> Result
%%       Query = any()
%%       Name = list() | binary()
%%       Result = any()
%% @doc Remove a filter from a query
remove_filter(Query, Name) when is_list(Name) ->
  remove_filter(Query, list_to_binary(Name));
remove_filter(#giza_query{filters=Filters}=Query, Name) when is_binary(Name) ->
  Query#giza_query{filters=proplists:delete(Name, Filters)}.
%% Not supported yet
%% command(Query) ->
%%   Query#giza_query.command.

%% command(Query, Command) when is_integer(Command) ->
%%   set_query_field(command, Query, Command).

%% @hidden
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
 end;
set_query_field(host, Query, Host) ->
  Query#giza_query{host=Host};
set_query_field(port, Query, Port) ->
  Query#giza_query{port=Port};
set_query_field(limit, Query, Limit) ->
  Query#giza_query{limit=Limit};
set_query_field(min_id, Query, MinId) ->
  Query#giza_query{min_id=MinId};
set_query_field(max_id, Query, MaxId) ->
  Query#giza_query{max_id=MaxId};
set_query_field(offset, Query, Offset) ->
  Query#giza_query{offset=Offset}.
