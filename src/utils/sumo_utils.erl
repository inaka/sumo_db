%%% @doc Set of useful functions for the app
-module(sumo_utils).

%%% API
-export([
  doc_filter_fields_by/2,
  doc_transform/2,
  transform_conditions/4,
  is_datetime/1,
  keyfind/2,
  keyfind/3,
  is_key/2,
  pipe/2,
  text/2,
  to_bin/1,
  to_atom/1,
  to_list/1,
  to_int/1,
  to_float/1,
  to_boolean/1,
  to_datetime/1
]).

-type keyword()  :: [{atom(), term()}].
-type datetime() :: calendar:date() | calendar:datetime().

%%%=============================================================================
%%% API
%%%=============================================================================

-spec doc_transform(Fun, Doc1) -> Doc2 when
  Fun         :: fun((FieldType, FieldName, Value1, FieldAttrs) -> Value2),
  FieldType   :: sumo:field_type(),
  FieldName   :: sumo:field_name(),
  Value1      :: sumo:field_value(),
  FieldAttrs  :: sumo:field_attrs(),
  Value2      :: sumo:field_value(),
  Doc1        :: sumo_internal:doc(),
  Doc2        :: sumo_internal:doc().
doc_transform(Fun, Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  lists:foldl(fun(Field, Acc) ->
    FieldType = sumo_internal:field_type(Field),
    FieldName = sumo_internal:field_name(Field),
    FieldValue = sumo_internal:get_field(FieldName, Doc),
    FieldAttrs = sumo_internal:field_attrs(Field),
    NewValue = Fun(FieldType, FieldName, FieldValue, FieldAttrs),
    sumo_internal:set_field(FieldName, NewValue, Acc)
  end, Doc, SchemaFields).

-spec doc_filter_fields_by(DocNameOrDoc, FilteredFieldTypes) -> Response when
  DocNameOrDoc       :: atom() | sumo_internal:doc(),
  FilteredFieldTypes :: [sumo:field_type()],
  FieldName          :: sumo:field_name(),
  FieldType          :: sumo:field_type(),
  FieldValue         :: sumo:field_value(),
  ResTuple           :: {FieldName, FieldType, FieldValue},
  Response           :: [ResTuple].
doc_filter_fields_by(DocNameOrDoc, FilteredFieldTypes) when is_atom(DocNameOrDoc) ->
  filter_fields(DocNameOrDoc, undefined, FilteredFieldTypes);
doc_filter_fields_by(DocNameOrDoc, FilteredFieldTypes) ->
  DocName = sumo_internal:doc_name(DocNameOrDoc),
  filter_fields(DocName, DocNameOrDoc, FilteredFieldTypes).

%% @private
filter_fields(DocName, Doc, FilteredFieldTypes) ->
  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  lists:foldl(fun(Field, Acc) ->
    FieldType = sumo_internal:field_type(Field),
    case lists:member(FieldType, FilteredFieldTypes) of
      true -> [set_filter_response(Field, Doc) | Acc];
      _    -> Acc
    end
  end, [], SchemaFields).

%% @private
set_filter_response(Field, Doc) ->
  FieldType = sumo_internal:field_type(Field),
  FieldName = sumo_internal:field_name(Field),
  FieldValue = case Doc /= undefined of
    true -> sumo_internal:get_field(FieldName, Doc);
    _    -> undefined
  end,
  {FieldName, FieldType, FieldValue}.

-spec transform_conditions(Fun, DocName, Conditions, FieldTypes) -> Res when
  Fun        :: fun((DocSum) -> sumo:field_value()),
  DocSum     :: {sumo:field_type(), sumo:field_name(), sumo:field_value()},
  DocName    :: atom(),
  Conditions :: sumo:conditions(),
  FieldTypes :: [sumo:field_type()],
  Res        :: sumo:conditions().
transform_conditions(Fun, DocName, Conditions, FieldTypes) when is_list(Conditions) ->
  DTFields = doc_filter_fields_by(DocName, FieldTypes),
  lists:foldl(fun
    ({K, V}, Acc) when K == 'and'; K == 'or' ->
      [{K, transform_conditions(Fun, DocName, V, FieldTypes)} | Acc];
    ({'not', V}, Acc) ->
      [NotCond] = transform_conditions(Fun, DocName, [V], FieldTypes),
      [{'not', NotCond} | Acc];
    ({K, V} = KV, Acc) ->
      case lists:keyfind(K, 1, DTFields) of
        {K, FieldType, _} ->
          [{K, Fun({FieldType, K, V})} | Acc];
        false ->
          [KV | Acc]
      end;
    ({K, Op, V} = KV, Acc) ->
      case lists:keyfind(K, 1, DTFields) of
        {K, FieldType, _} ->
          [{K, Op, Fun({FieldType, K, V})} | Acc];
        false ->
          [KV | Acc]
      end;
    (Cond, Acc) ->
      [Cond | Acc]
  end, [], Conditions);
transform_conditions(Fun, DocName, Conditions, FieldTypes) ->
  transform_conditions(Fun, DocName, [Conditions], FieldTypes).

-spec is_datetime(calendar:date() | calendar:datetime()) -> boolean().
is_datetime({_, _, _} = Date) ->
  calendar:valid_date(Date);
is_datetime({{_, _, _} = Date, {H, M, S}}) ->
  calendar:valid_date(Date) and
    (H >= 0 andalso H =< 23) and
    (M >= 0 andalso M =< 59) and
    (S >= 0 andalso S =< 59);
is_datetime(_) ->
  false.

-spec keyfind(Key, KVList) -> Val | undefined when
  Key    :: term(),
  KVList :: [{Key, Val}],
  Val    :: term().
keyfind(Key, KVList) ->
  keyfind(Key, KVList, undefined).

-spec keyfind(Key, KVList, Default) -> Val | Default when
  Key     :: term(),
  KVList  :: [{Key, Val}],
  Val     :: term(),
  Default :: term().
keyfind(Key, KVList, Default) ->
  case lists:keyfind(Key, 1, KVList) of
    {Key, Value} -> Value;
    _            -> Default
  end.

-spec is_key(atom(), keyword()) -> boolean().
is_key(Key, Map) when is_atom(Key), is_map(Map) ->
  maps:is_key(Key, Map);
is_key(Key, Keyword) when is_atom(Key), is_list(Keyword) ->
  lists:keymember(Key, 1, Keyword).

-spec pipe(Initial, Pipeline) -> Return when
  Initial  :: term(),
  Module   :: module(),
  Fun      :: atom(),
  Args     :: [term()],
  FunSpec  :: {fun(), Args} | {Module, Fun, Args},
  Pipeline :: [FunSpec],
  Return   :: term().
pipe(Initial, Pipeline) ->
  lists:foldl(fun
    ({Module, Fun, Args}, Acc) ->
      apply(Module, Fun, [Acc | Args]);
    ({Fun, Args}, Acc) ->
      apply(Fun, [Acc | Args])
  end, Initial, Pipeline).

-spec text(string(), [term()]) -> binary().
text(Msg, Args) ->
  iolist_to_binary(io_lib:format(Msg, Args)).

-spec to_bin(Data :: term()) -> binary().
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
to_bin(Data) ->
  Data.

-spec to_atom(Data :: term()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

-spec to_list(Data :: term()) -> list().
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
to_list(Data) ->
  Data.

-spec to_int(Data :: term()) -> integer().
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

-spec to_float(Data :: term()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  case string:to_float(Data) of
    {error, no_float} -> list_to_integer(Data);
    {F, _Rest}        -> F
  end;
to_float(Data) when is_integer(Data) ->
  Data / 1;
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data) / 1;
to_float(Data) ->
  Data.

-spec to_boolean(any()) -> boolean().
to_boolean(Data) ->
  case to_atom(Data) of
    V when V == true; V == false ->
      V;
    _ ->
      error({badarg, Data})
  end.

-spec to_datetime(any()) -> datetime() | no_return().
to_datetime(Data) ->
  case is_datetime(Data) of
    true -> Data;
    _    -> error({badarg, Data})
  end.
