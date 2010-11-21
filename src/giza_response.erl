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

%% @hidden
-module(giza_response).

-author("Kevin A. Smith <kevin@hypotheticalabs.com>").

-include("giza.hrl").
-include("giza_internal.hrl").

-export([parse_query/1, parse_update/1]).

%% @doc Parses the response from searchd server
parse_query(Sock) ->
  %% Ignore aggregate status, version, and response size
  gen_tcp:recv(Sock, 8),
  case giza_protocol:read_number(Sock, 32) of
    Status when Status == ?SEARCHD_OK; Status == ?SEARCHD_WARN ->
      {ok, parse_results(Sock, Status)};
    _ ->
      {error, giza_protocol:read_lp_string(Sock)}
  end.

parse_update(Sock) ->
  case giza_protocol:read_number(Sock, 16) of
    ?SEARCHD_OK ->
      {ok, parse_update_results(Sock)};
    ?SEARCHD_WARN ->
      {Warning, Updated} = parse_update_warning(Sock),
      {warning, Warning, Updated};
    ?SEARCHD_ERR ->
      {error, giza_protocol:read_lp_string(Sock)}
  end.

%% @hidden
%% Internal functions
parse_update_results(Sock) ->
  %% throw away version number for now
  giza_protocol:read_number(Sock, 16),
  giza_protocol:read_number(Sock, 32),
  giza_protocol:read_number(Sock, 32).

parse_update_warning(Sock) ->
  %% throw away version number for now
  giza_protocol:read_number(Sock, 16),
  giza_protocol:read_number(Sock, 32),
  Warning = giza_protocol:read_lp_string(Sock),
  Updates = giza_protocol:read_number(Sock, 32),
  {Warning, Updates}.

parse_results(Sock, Status) ->
  Result = case Status of
    ?SEARCHD_OK ->
      #giza_query_result{status=?SEARCHD_OK};
    ?SEARCHD_WARN ->
      Msg = giza_protocol:read_lp_string(Sock),
      #giza_query_result{status=?SEARCHD_WARN, warning=Msg}
  end,
  Fields = giza_protocol:map(
    fun(S) -> giza_protocol:read_lp_string(S) end,
    Sock),
  Attrs = giza_protocol:map(
    fun(S) -> {giza_protocol:read_lp_string(S),
               giza_protocol:read_number(S, 32)} end,
    Sock),
  HitCount = giza_protocol:read_number(Sock, 32),
  IdBitSize = case giza_protocol:read_number(Sock, 32) of 
    0 -> 32;
    1 -> 64
  end,
  Matches = read_matches(Sock, HitCount, IdBitSize,  Attrs, []),
  Total = giza_protocol:read_number(Sock, 32),
  TotalFound = giza_protocol:read_number(Sock, 32),
  Time = giza_protocol:read_number(Sock, 32),
  WordCount = giza_protocol:read_number(Sock, 32),
  Words = read_words(Sock, WordCount, []),
  Result#giza_query_result{
    matches=Matches,
    attrs=Attrs,
    fields=Fields,
    words=Words,
    total=Total,
    total_found=TotalFound,
    time=Time/1000.0}.

read_matches(_Sock, 0, _IdBitSize, _Attrs, Accum) ->
  lists:reverse(Accum);
read_matches(Sock, HitCount, IdBitSize, AttrDefs, Accum) ->
  DocId = giza_protocol:read_number(Sock, IdBitSize),
  Weight = giza_protocol:read_number(Sock, 32),
  Attrs = [read_attr(A, Sock) || A <- AttrDefs],
  read_matches(Sock, HitCount - 1, IdBitSize, AttrDefs,
               [{DocId, [{doc_id, DocId},
                         {weight, Weight},
                         {attrs, Attrs}]}|Accum]).

read_words(_Sock, 0, Accum) ->
  Accum;
read_words(Sock, WordCount, Accum) ->
  Word = giza_protocol:read_lp_string(Sock),
  Docs = giza_protocol:read_number(Sock, 32),
  Hits = giza_protocol:read_number(Sock, 32),
  read_words(Sock, WordCount - 1, [{Word, Docs, Hits} | Accum]).

read_attr({Name, ?SPHINX_ATTR_FLOAT}, Sock) ->
  {Name, giza_protocol:read_float(Sock, 32)};
read_attr({Name, ?SPHINX_ATTR_INTEGER}, Sock) ->
  {Name, giza_protocol:read_number(Sock, 32)};
read_attr({Name, ?SPHINX_ATTR_MULTI}, Sock) ->
  Count = giza_protocol:read_number(Sock, 32),
  Values = read_multi_values(Count, Sock, []),
  {Name, Values};
read_attr({Name, ?SPHINX_ATTR_TIMESTAMP}, Sock) ->
  {Name, giza_protocol:read_timestamp(Sock)};
read_attr({Name, _}, Sock) ->
  {Name, giza_protocol:read_number(Sock, 32)}.

read_multi_values(0, _Sock, Accum) ->
  lists:reverse(Accum);
read_multi_values(Count, Sock, Accum) ->
  read_multi_values(Count - 1, Sock, [giza_protocol:read_number(Sock, 32)|Accum]).
