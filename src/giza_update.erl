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

-module(giza_update).

-include("giza.hrl").

-compile([export_all]).
-export([new/1, new/3, attributes/1, add_attribute/2, add_attributes/2, remove_attribute/2]).
-export([docs/1, add_doc/3, remove_doc/2]).
-export([host/1, host/2, port/1, port/2]).
-export([to_bytes/1]).

new(IndexName) ->
  #giza_update{index=IndexName}.

new(IndexName, Host, Port) when is_list(Host),
                                is_number(Port) ->
  #giza_update{index=IndexName, host=Host, port=Port}.

attributes(#giza_update{attributes=Fields}=_Update) ->
  Fields.

add_attribute(Update, FieldName) when is_list(FieldName) ->
  add_attribute(Update, list_to_binary(FieldName));
add_attribute(#giza_update{attributes=Fields}=Update, FieldName) ->
  case giza_util:index(Fields, FieldName) of
    -1 ->
      Update#giza_update{attributes=lists:flatten([Fields, FieldName])};
    _ ->
      Update
  end.

add_attributes(Update, FieldNames) ->
  lists:foldl(fun(Field, U) ->
                  add_attribute(U, Field) end,
              Update,
              FieldNames).

remove_attribute(Update, FieldName) when is_list(FieldName) ->
  remove_attribute(Update, list_to_binary(FieldName));
remove_attribute(#giza_update{attributes=Fields, updates=Updates}=Update, FieldName) ->
  case giza_util:index(Fields, FieldName) of
    -1 ->
      Update;
    FieldPos ->
      Update#giza_update{attributes=lists:filter(fun(F) -> not(F =:= FieldName) =:= true end,
                                             Fields),
                         updates=clean_updates(FieldPos, Updates)}
  end.

docs(#giza_update{updates=Updates}=_Update) ->
  Updates.

add_doc(#giza_update{attributes=Fields, updates=Updates}=Update, DocId, Attributes) when is_number(DocId),
                                                                                            is_list(Attributes) ->
  if
    length(Attributes) == length(Fields) ->
      Update#giza_update{updates=[{DocId, Attributes}|Updates]};
    true ->
      throw({error, missing_fields})
  end.

remove_doc(#giza_update{updates=Updates}=Update, DocId) when is_number(DocId) ->
  Update#giza_update{updates=proplists:delete(DocId, Updates)}.

to_bytes(Update) ->
  Commands = to_commands(Update),
  giza_protocol:commands_to_bytes(Commands).

%% @spec host(Update) -> Result
%%       Update = any()
%%       Result = string()
%% @doc Get the current target host
host(Update) ->
  Update#giza_update.host.

%% @spec host(Update, Host) -> Result
%%       Update = any()
%%       Host = string()
%%       Result = any()
%% @doc Set the target host
host(Update, Host) when is_list(Host) ->
  Update#giza_update{host=Host}.

%% @spec port(Update) -> Result
%%       Update = any()
%%       Result = integer()
%% @doc Get the current target port
port(Update) ->
  Update#giza_update.port.

%% @spec port(Update, Port) -> Result
%%       Update = any()
%%       Port = integer()
%%       Result = any()
%% @doc Set the current target port
port(Update, Port) when is_number(Port) ->
  Update#giza_update{port=Port}.

%% Internal functions
clean_updates(FieldPos, Updates) ->
  clean_updates(FieldPos, Updates, []).

clean_updates(_FieldPos, [], Accum) ->
  Accum;
clean_updates(FieldPos, [{DocId, Values}|T], Accum) ->
  Before = lists:sublist(Values, FieldPos - 1),
  After = lists:sublist(Values, FieldPos + 1, length(Values)),
  case Before =:= [] andalso After =:= [] of
    true ->
      clean_updates(FieldPos, T, Accum);
    false ->
      clean_updates(FieldPos, T, [{DocId, lists:flatten([Before, After])}|Accum])
  end.

to_commands(Update) ->
  lists:flatten([{string, Update#giza_update.index},
                 process_attributes(Update#giza_update.attributes),
                 {32, length(Update#giza_update.updates)},
                 process_updates(Update#giza_update.updates)]).

process_attributes(Attributes) ->
  [{32, length(Attributes)},
   lists:map(fun(A) -> {string, A} end, Attributes)].

process_updates([]) ->
  [{32, 0}];
process_updates(Updates) ->
  process_updates(Updates, []).

process_updates([], Accum) ->
  lists:flatten(lists:reverse(Accum));
process_updates([{DocId, Updates}|T], Accum) ->
  PU = lists:reverse(lists:foldl(fun(U, Acc) -> [{32, U}|Acc] end,
                                 [{64, DocId}], Updates)),
  process_updates(T, [PU|Accum]).
