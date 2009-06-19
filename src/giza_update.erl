-module(giza_update).

-include("giza.hrl").

-export([new/1, new/3, fields/1, add_field/2, add_fields/2, remove_field/2]).
-export([docs/1, add_doc/3, remove_doc/2]).
-export([host/1, host/2, port/1, port/2]).
-export([to_bytes/1]).

new(IndexName) ->
  #giza_update{index=IndexName}.

new(IndexName, Host, Port) when is_list(Host),
                                is_number(Port) ->
  #giza_update{index=IndexName, host=Host, port=Port}.

fields(#giza_update{fields=Fields}=_Update) ->
  Fields.

add_field(Update, FieldName) when is_list(FieldName) ->
  add_field(Update, list_to_binary(FieldName));
add_field(#giza_update{fields=Fields}=Update, FieldName) ->
  case giza_util:index(Fields, FieldName) of
    -1 ->
      Update#giza_update{fields=lists:flatten([Fields, FieldName])};
    _ ->
      Update
  end.

add_fields(Update, FieldNames) ->
  lists:foldl(fun(Field, U) ->
                  add_field(U, Field) end,
              Update,
              FieldNames).

remove_field(Update, FieldName) when is_list(FieldName) ->
  remove_field(Update, list_to_binary(FieldName));
remove_field(#giza_update{fields=Fields, updates=Updates}=Update, FieldName) ->
  case giza_util:index(Fields, FieldName) of
    -1 ->
      Update;
    FieldPos ->
      Update#giza_update{fields=lists:filter(fun(F) -> not(F =:= FieldName) =:= true end,
                                             Fields),
                         updates=clean_updates(FieldPos, Updates)}
  end.

docs(#giza_update{updates=Updates}=_Update) ->
  Updates.

add_doc(#giza_update{fields=Fields, updates=Updates}=Update, DocId, Attributes) when is_number(DocId),
                                                                                            is_list(Attributes) ->
  if
    length(Attributes) == length(Fields) ->
      Update#giza_update{updates=[{DocId, Attributes}|Updates]};
    true ->
      throw({error, missing_fields})
  end.

remove_doc(#giza_update{updates=Updates}=Update, DocId) when is_number(DocId) ->
  Update#giza_update{updates=proplists:delete(DocId, Updates)}.

to_bytes(#giza_update{index=Index, updates=Updates}=_Update) ->
  ok.

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
