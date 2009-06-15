-module(giza_datetime).

-define(EPOCH_BASE, 62167219200).

-export([to_timestamp/1, from_timestamp/1]).

to_timestamp({_, _, _}=Now) ->
  to_timestamp(calendar:now_to_universal_time(Now));

to_timestamp(DateTime) ->
  TS = calendar:datetime_to_gregorian_seconds(DateTime),
  TS - ?EPOCH_BASE.

from_timestamp(EpochTimestamp) ->
  calendar:gregorian_seconds_to_datetime(EpochTimestamp + ?EPOCH_BASE).
