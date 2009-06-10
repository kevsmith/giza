#!/usr/bin/env escript
%%! -pz t -pz ebin -sasl errlog_type error -boot start_sasl

main(_) ->
  giza_t_003:start().