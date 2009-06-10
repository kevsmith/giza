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

host(Query) ->
  Query#giza_query.host.

host(Query, Host) when is_list(Host) ->
  set_query_field(host, Query, Host).

port(Query) ->
  Query#giza_query.port.

port(Query, Port) when is_number(Port) ->
  set_query_field(port, Query, Port).

limit(Query) ->
  Query#giza_query.limit.

limit(Query, Limit) when is_number(Limit) ->
  set_query_field(limit, Query, Limit).

offset(Query) ->
  Query#giza_query.offset.

offset(Query, Offset) ->
  set_query_field(offset, Query, Offset).

min_id(Query) ->
  Query#giza_query.min_id.

min_id(Query, MinId) ->
  set_query_field(min_id, Query, MinId).

max_id(Query) ->
  Query#giza_query.max_id.

max_id(Query, MaxId) ->
  set_query_field(max_id, Query, MaxId).

%% Not supported yet
%% command(Query) ->
%%   Query#giza_query.command.

%% command(Query, Command) when is_integer(Command) ->
%%   set_query_field(command, Query, Command).

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
