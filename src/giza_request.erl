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

-module(giza_request).

-author("Kevin A. Smith <kevin@hypotheticalabs.com>").

-include("giza.hrl").
-include("giza_internal.hrl").

-export([send/1]).

%% @spec send(Query) -> Result
%%       Query = any()
%%       Result = [{number(), [any()]}]
%% @doc Sends a giza query to a searchd server
send(Query) ->
  case connect(Query) of
    {ok, Sock} ->
      case write_command(Sock, Query) of
        ok ->
          write_query(Sock, Query),
          Results = giza_response:parse(Sock),
          catch gen_tcp:close(Sock),
          Results;
        CommandError ->
         CommandError
      end;
    ConnectError ->
      ConnectError
  end.

%% Internal functions
write_query(Sock, Query) ->
  {Bytes, Size} =  giza_query:to_bytes(Query),
  giza_protocol:write_number(Sock, Size, 32),
  gen_tcp:send(Sock, Bytes).

write_command(Sock, Query) ->
  giza_protocol:write_number(Sock, Query#giza_query.command, 16),
  giza_protocol:write_number(Sock, Query#giza_query.command_version, 16).

connect(Query) ->
  case gen_tcp:connect(Query#giza_query.host,
                       Query#giza_query.port,
                       [binary, {packet, raw},
                        {active, false}]) of
    {ok, Sock} ->
      case verify_version(Sock) of
        ok ->
          giza_protocol:write_number(Sock, ?SPHINX_MAJOR_PROTO, 32),
          {ok, Sock};
        VerifyError ->
          gen_tcp:close(Sock),
          VerifyError
      end;
    ConnectError ->
      ConnectError
  end.

verify_version(Sock) ->
  {ok, RawVersion} = gen_tcp:recv(Sock, 4),
  case giza_protocol:binary_to_number(RawVersion, 32, true) of
    ?SPHINX_MAJOR_PROTO ->
      ok;
    BadVersion ->
      {error, {bad_version, BadVersion}}
  end.
