-module(giza_t_002).

-export([start/0]).

start() ->
  etap:plan(7),
  A = <<1,0,0,0>>,
  etap:is(1, giza_protocol:binary_to_number(A, 32, true), "Convert network order to correct endian-ness"),
  etap:is(<<1, 1>>, giza_protocol:convert_number(257, 16), "Break number into 2 bytes"),
  etap:is(<<0, 1, 1, 1>>, giza_protocol:convert_number(65793, 32), "Break number info 4 bytes"),
  etap:is([<<0,0,0,0>>, <<>>], giza_protocol:convert_string(<<>>), "Convert empty string to length-prefixed form"),
  etap:is([<<0,0,0,12>>, <<"hello, world">>], giza_protocol:convert_string(<<"hello, world">>), "Convert string to length-prefixed form"),
  etap:is({<<"hello, world">>, <<>>}, giza_protocol:convert_lp_string(<<0,0,0,12,"hello, world">>), "Convert length-prefixed string to binary"),
  [Size, String] = giza_protocol:convert_string(<<"Now is the winter of our discontent">>),
  etap:is({<<"Now is the winter of our discontent">>, <<1,2,3,4,5>>}, giza_protocol:convert_lp_string(list_to_binary([Size, String, <<1,2,3,4,5>>])),
          "Read in length-prefixed string and leave the correct number of bytes at the end"),
  etap:end_tests().
