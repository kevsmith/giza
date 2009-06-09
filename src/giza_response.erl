-module(giza_response).

-include("giza.hrl").
-include("giza_internal.hrl").

-export([parse/1]).

parse(Sock) ->
  %% Ignore aggregate status, version, and response size
  gen_tcp:recv(Sock, 8),
  case giza_protocol:read_number(Sock, 32) of
    ?SEARCHD_OK ->
      parse_results(Sock);
    _ ->
      {error, giza_protocol:read_lp_string(Sock)}
  end.
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
  io:format("IdBitSize: ~p~n", [IdBitSize]),
  read_matches(Sock, HitCount, IdBitSize,  Attrs, []).

read_matches(_Sock, 0, _IdBitSize, _Attrs, Accum) ->
  lists:usort(lists:reverse(Accum));
read_matches(Sock, HitCount, IdBitSize, AttrDefs, Accum) ->
  DocId = giza_protocol:read_number(Sock, IdBitSize),
  io:format("DocId: ~p~n", [DocId]),
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
read_attr({Name, _}, Sock) ->
  {Name, giza_protocol:read_number(Sock, 32)}.

read_multi_values(0, _Sock, Accum) ->
  lists:reverse(Accum);
read_multi_values(Count, Sock, Accum) ->
  read_multi_values(Count - 1, Sock, [giza_protocol:read_number(Sock, 32)|Accum]).
