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

-module(giza_datetime).

-author("Kevin A. Smith <kevin@hypotheticalabs.com>").

-define(EPOCH_BASE, 62167219200).

-export([to_timestamp/1, from_timestamp/1]).

%% @doc Encode the tuple returned from erlang:now/0 into a Unix epoch timestamp
to_timestamp({_, _, _}=Now) ->
  to_timestamp(calendar:now_to_universal_time(Now));

%% @doc Encode an Erlang datetime tuple into a Unix epoch timestamp
to_timestamp(DateTime) ->
  TS = calendar:datetime_to_gregorian_seconds(DateTime),
  TS - ?EPOCH_BASE.

%% @doc Convert an Unix epoch timestamp into the equivalent Unix datetime tuple
from_timestamp(EpochTimestamp) ->
  calendar:gregorian_seconds_to_datetime(EpochTimestamp + ?EPOCH_BASE).
