-module(giza_protocol).

-export([binary_to_number/2, binary_to_number/3]).
-export([convert_number/2, convert_string/1, convert_lp_string/1]).
-export([write_number/3, write_string/2]).
-export([read_number/2, read_float/2,  read_lp_string/1, read_lp_string_list/1]).
-export([map/2]).

binary_to_number(Data, Size) ->
  binary_to_number(Data, Size, false).

binary_to_number(Data, Size, IsBig) when is_binary(Data) ->
  Value = if
            IsBig =:= true ->
              <<V:Size/little>> = Data,
              V;
            true ->
              <<V:Size>> = Data,
              V
          end,
  Value.

convert_number(0, 16) ->
  <<0, 0>>;
convert_number(0, 32) ->
  <<0, 0, 0, 0>>;
convert_number(Value, 16) ->
  <<V1:8, V2:8>> = <<Value:16>>,
  <<V1, V2>>;
convert_number(Value, 32) ->
  <<V1:8, V2:8, V3:8, V4:8>> = <<Value:32>>,
  <<V1, V2, V3, V4>>.

convert_string(<<>>) ->
  [convert_number(0, 32), <<>>];
convert_string(Value) ->
  [convert_number(size(Value), 32), Value].

convert_lp_string(RawString) ->
  {<<Size:32>>, Rest} = erlang:split_binary(RawString, 4),
  if
    size(Rest) == Size ->
      {Rest, <<>>};
    true ->
      erlang:split_binary(Rest, Size)
  end.

read_number(Sock, Size) ->
  {ok, N} = gen_tcp:recv(Sock, (Size div 8)),
  <<N1:Size>> = N,
  N1.

read_lp_string(Sock) ->
  Length = read_number(Sock, 32),
  {ok, String} = gen_tcp:recv(Sock, Length),
  String.

map(Fun, Sock) ->
  Count = read_number(Sock, 32),
  do_map(Count, Fun, Sock, []).

do_map(0, _Fun, _Sock, Accum) ->
  lists:reverse(Accum);
do_map(Count, Fun, Sock, Accum) ->
  do_map(Count - 1, Fun, Sock, [Fun(Sock)|Accum]).

read_lp_string_list(Sock) ->
  Count = read_number(Sock, 32),
  read_lp_string_list(Count, Sock, []).

read_lp_string_list(0, _Sock, Accum) ->
  lists:reverse(Accum);
read_lp_string_list(Count, Sock, Accum) ->
  read_lp_string_list(Count - 1, Sock, [read_lp_string(Sock)|Accum]).

read_float(Sock, 32) ->
  {ok, Data} = gen_tcp:recv(Sock, 4),
  <<Value:32/float>> = Data,
  Value.

write_number(Sock, Value, Size) when is_number(Value),
                                     is_number(Size),
                                     is_port(Sock) ->
  gen_tcp:send(Sock, convert_number(Value, Size)).

write_string(Sock, String) when is_binary(String) ->
  gen_tcp:send(Sock, convert_string(String)).
