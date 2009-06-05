-module(giza_integ_001).

-include("giza.hrl").

-export([start/0]).

start() ->
  etap:plan(1),
  Query = giza_query:new(),
  Q1 = giza_query:query_string(Query, "apa"),
  etap:ok(<<0,0,0,0>> == giza_request:send(Q1), "Check version number is ok"),
  etap:end_tests().
