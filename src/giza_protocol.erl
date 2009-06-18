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

-export([binary_to_number/2, binary_to_number/3]).
-export([convert_number/2, convert_string/1, convert_lp_string/1]).
-export([write_number/3, write_string/2]).
-export([read_number/2, read_float/2,  read_lp_string/1, read_lp_string_list/1]).
-export([read_timestamp/1, map/2]).

%% @spec binary_to_number(Data, Size) -> Result
%%       Data = binary()
%%       Size = 16 | 32
%%       Result = integer()
%% @doc Convert binary to number
binary_to_number(Data, Size) ->
  binary_to_number(Data, Size, false).

%% @spec binary_to_number(Data, Size, IsBig) -> Result
%%       Data = binary()
%%       Size = 16 | 32
%%       IsBig = true | false
%%       Result = integer()
%% @doc Convert binary to number
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

%% @spec convert_number(Value, Size) -> Result
%%       Value = integer()
%%       Size = 16 | 32
%%       Result = binary()
%% @doc Convert number to binary
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

%% @spec convert_string(String) -> Result
%%       String = binary()
%%       Result = list(binary())
%% @doc Convert an Erlang binary string to a Sphinx string
convert_string(<<>>) ->
  [convert_number(0, 32), <<>>];
convert_string(Value) ->
  [convert_number(size(Value), 32), Value].

%% @spec convert_lp_string(RawString) -> Result
%%       RawString = binary()
%%       Result = {binary(), binary()}
%% @doc Convert a Sphinx string to an Erlang binary string
convert_lp_string(RawString) ->
  {<<Size:32>>, Rest} = erlang:split_binary(RawString, 4),
  if
    size(Rest) == Size ->
      {Rest, <<>>};
    true ->
      erlang:split_binary(Rest, Size)
  end.

%% @spec read_number(Sock, Size) -> Result
%%       Sock = port()
%%       Size = 16 | 32
%%       Result = integer()
%% @doc Read number from a TCP/IP socket
read_number(Sock, Size) ->
  {ok, N} = gen_tcp:recv(Sock, (Size div 8)),
  <<N1:Size>> = N,
  N1.

%% @spec read_lp_string(Sock) -> Result
%%       Sock = port()
%%       Result = binary()
%% @doc Read a Sphinx string rom  a TCP/IP/Socket
read_lp_string(Sock) ->
  Length = read_number(Sock, 32),
  {ok, String} = gen_tcp:recv(Sock, Length),
  String.

%% @spec map(Fun, Sock) -> Result
%%       Fun = function()
%%       Sock = port()
%%       Result = [any()]
%% @doc Map over an incoming data stream and process it
map(Fun, Sock) ->
  Count = read_number(Sock, 32),
  do_map(Count, Fun, Sock, []).

do_map(0, _Fun, _Sock, Accum) ->
  lists:reverse(Accum);
do_map(Count, Fun, Sock, Accum) ->
  do_map(Count - 1, Fun, Sock, [Fun(Sock)|Accum]).

%% @spec read_lp_string_list(Sock) -> Result
%%       Sock = port()
%%       Result = [binary()]
%% @doc Read a list of Sphinx strings
read_lp_string_list(Sock) ->
  Count = read_number(Sock, 32),
  read_lp_string_list(Count, Sock, []).

read_lp_string_list(0, _Sock, Accum) ->
  lists:reverse(Accum);
read_lp_string_list(Count, Sock, Accum) ->
  read_lp_string_list(Count - 1, Sock, [read_lp_string(Sock)|Accum]).

%% @spec read_float(Sock, Size) -> Result
%%       Sock = port()
%%       Size = 32
%%       Result = float()
%% @doc Read a float froma TCP/IP socket
read_float(Sock, 32) ->
  {ok, Data} = gen_tcp:recv(Sock, 4),
  <<Value:32/float>> = Data,
  Value.

%% @spec write_number(Sock, Value, Size) -> Result
%%       Sock = port()
%%       Value = integer()
%%       Size = 16 | 32
%%       Result = ok | {error, atom()}
%% @doc Write a number as a Sphinx encoded value to a TCP/IP socket
write_number(Sock, Value, Size) when is_number(Value),
                                     is_number(Size),
                                     is_port(Sock) ->
  gen_tcp:send(Sock, convert_number(Value, Size)).

%% @spec write_string(Sock, String) -> Result
%%       Sock = port()
%%       String = binary()
%%       Result = ok | {error, atom()}
%% @doc Write a string as a Sphinx encoded value to a TCP/IP socket
write_string(Sock, String) when is_binary(String) ->
  gen_tcp:send(Sock, convert_string(String)).

%% @spec read_timestamp(Sock) -> Result
%%       Sock = port()
%%       Result ={{integer(), integer(), integer()}, {integer(), integer(), integer()}}
%% @doc Read an Unix epoch datetime from a socket and convert
%%      it into an Erlang datetime tuple
read_timestamp(Sock) ->
  Number = read_number(Sock, 32),
  giza_datetime:from_timestamp(Number).
