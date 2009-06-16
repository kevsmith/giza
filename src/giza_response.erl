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

-export([parse/1]).

%% @doc Parses the response from searchd server
parse(Sock) ->
  %% Ignore aggregate status, version, and response size
  gen_tcp:recv(Sock, 8),
  case giza_protocol:read_number(Sock, 32) of
    ?SEARCHD_OK ->
      {ok, parse_results(Sock)};
    _ ->
      {error, giza_protocol:read_lp_string(Sock)}
  end.

%% @hidden
%% Internal functions
parse_results(Sock) ->
  _Fields = giza_protocol:map(fun(S) -> giza_protocol:read_lp_string(S) end, Sock),
  Attrs = giza_protocol:map(fun(S) -> {giza_protocol:read_lp_string(S),
                                       giza_protocol:read_number(S, 32)} end, Sock),
  HitCount = giza_protocol:read_number(Sock, 32),
  IdBitSize = case giza_protocol:read_number(Sock, 32) of
                0 ->
                  32;
                1 ->
                  64
              end,
  read_matches(Sock, HitCount, IdBitSize,  Attrs, []).

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
