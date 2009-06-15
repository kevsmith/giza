-module(giza_t_004).

-export([start/0]).

start() ->
  etap:plan(2),
  etap:is({{2009, 2, 13}, {23, 31, 30}}, giza_datetime:from_timestamp(1234567890), "Converting to Erlang datetime from epoch timestamps"),
  etap:is(1234567890, giza_datetime:to_timestamp({{2009, 2, 13}, {23, 31, 30}}), "Convert from Erlang datetimes to epoch timestamp"),
  etap:end_tests().
