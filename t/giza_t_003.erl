-module(giza_t_003).

-export([start/0]).

start() ->
  etap:plan(17),
  Q = giza_query:new(),
  etap:is("localhost", giza_query:host(Q), "Can get host from giza_query"),
  Q1 = giza_query:host(Q, "search.foo.com"),
  etap:is("search.foo.com", giza_query:host(Q1), "Can set host from giza_query"),
  etap:is(3312, giza_query:port(Q1), "Can get port from giza_query"),
  Q2 = giza_query:port(Q1, 5512),
  etap:is(5512, giza_query:port(Q2), "Can set port from giza_query"),
  etap:is(<<>>, giza_query:index(Q2), "Can get querystring from giza_query"),
  Q3 = giza_query:index(Q2, "foo"),
  etap:is(<<"foo">>, giza_query:index(Q3), "Can set querystring from giza_query"),
  etap:is(25, giza_query:limit(Q3), "Can get limit from giza_query"),
  Q4 = giza_query:limit(Q3, 50),
  etap:is(50, giza_query:limit(Q4), "Can set limit from giza_query"),
  etap:is(0, giza_query:offset(Q4), "Can get offset from giza_query"),
  Q5 = giza_query:offset(Q4, 5),
  etap:is(5, giza_query:offset(Q5), "Can set offset from giza_query"),
  etap:is(0, giza_query:min_id(Q5), "Can get min id from giza_query"),
  Q6 = giza_query:min_id(Q5, 5),
  etap:is(5, giza_query:min_id(Q6), "Can set min id from giza_query"),
  etap:is(0, giza_query:max_id(Q6), "Can get max id from giza_query"),
  Q7 = giza_query:max_id(Q6, 10),
  etap:is(10, giza_query:max_id(Q7), "Can set max id from giza_query"),
  etap:is([], giza_query:filters(Q7), "Filters defaults to empty list"),
  Q8 = giza_query:add_filter(Q7, "wibble", [23,17]),
  etap:is([{<<"wibble">>, [23,17]}], giza_query:filters(Q8), "Adding filters works"),
  Q9 = giza_query:remove_filter(Q8, "wibble"),
  etap:is([], giza_query:filters(Q9), "Deleting filters works"),
  etap:end_tests().
