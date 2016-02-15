%%% Set of useful functions for the app
-module(sumo_utils).

-export([fields_from_doc/1]).
-export([to_bin/1, to_atom/1, to_list/1, to_int/1, to_float/1]).

-spec fields_from_doc(Doc::sumo_internal:doc()) -> [tuple()].
fields_from_doc(Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  lists:foldl(fun(Field, Acc) ->
    FieldType = sumo_internal:field_type(Field),
    FieldName = sumo_internal:field_name(Field),
    FieldValue = sumo_internal:get_field(FieldName, Doc),
    [{FieldName, FieldType, FieldValue} | Acc]
  end, [], SchemaFields).

-spec to_bin(Data::term()) -> binary().
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_binary(erlang:phash2(Data));
to_bin(undefined) -> <<>>;
to_bin(Data) ->
  Data.

-spec to_atom(Data::term()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

-spec to_list(Data::term()) -> list().
to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_list(erlang:phash2(Data));
to_list(undefined) -> "";
to_list(Data) ->
  Data.

-spec to_int(Data::term()) -> integer().
to_int(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
  list_to_integer(Data);
to_int(Data) when is_float(Data) ->
  trunc(Data);
to_int(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_int(Data) ->
  Data.

-spec to_float(Data::term()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_integer(Data) ->
  Data / 1;
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data) / 1;
to_float(Data) ->
  Data.
