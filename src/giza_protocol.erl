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

-module(giza_protocol).

-author("Kevin A. Smith <kevin@hypotheticalabs.com>").

-define(EPOCH_BASE, 62167219200).

-export([binary_to_number/2, binary_to_number/3]).
-export([convert_number/2, convert_string/1, convert_lp_string/1]).
-export([write_number/3, write_string/2]).
-export([read_number/2, read_float/2,  read_lp_string/1, read_lp_string_list/1]).
-export([read_timestamp/1, map/2]).

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

read_timestamp(Sock) ->
  EpochTimestamp = read_number(Sock, 32),
  calendar:gregorian_seconds_to_datetime(EpochTimestamp + ?EPOCH_BASE).

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
