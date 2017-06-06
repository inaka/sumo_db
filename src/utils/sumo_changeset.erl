%%%-------------------------------------------------------------------
%%% @doc
%%% Based on Elixir `Ecto.Changeset'.
%%%
%%% Changesets allow filtering, casting, validation and definition of
%%% constraints when manipulating sumo models.
%%%
%%% @reference See
%%% <a href="https://hexdocs.pm/ecto/Ecto.Changeset.html">Ecto.Changeset</a>
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(sumo_changeset).

%% Properties
-export([
  schema/1,
  store/1,
  data/1,
  params/1,
  errors/1,
  changes/1,
  is_valid/1,
  types/1,
  required/1
]).

%% API
-export([
  add_error/3, add_error/4,
  apply_changes/1,
  cast/3, cast/4,
  change/2, change/3,
  get_field/2, get_field/3,
  put_change/3,
  get_change/2, get_change/3,
  delete_change/2,
  validate_change/3,
  validate_required/2,
  validate_inclusion/3,
  validate_number/3,
  validate_length/3,
  validate_format/3
]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type keyword() :: [{atom(), term()}].

-type error() :: {binary(), keyword()}.

-type errors() :: [{atom(), error()}].

-type key() :: atom() | binary().

-type params() :: #{key() => term()}.

-type kv_map() :: #{atom() => term()}.

%% Changeset definition
-opaque changeset() :: #{
  schema   => sumo:schema_name(),
  store    => atom(),
  data     => sumo:model() | undefined,
  params   => params() | undefined,
  errors   => errors() | undefined,
  changes  => kv_map() | undefined,
  is_valid => boolean(),
  types    => kv_map() | undefined,
  required => [atom()] | undefined
}.

%% Exported types
-export_type([
  changeset/0,
  params/0,
  keyword/0
]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

-spec schema(changeset()) -> sumo:schema_name().
schema(#{schema := Value}) ->
  Value.

-spec store(changeset()) -> atom().
store(#{store := Value}) ->
  Value.

-spec data(changeset()) -> sumo:model().
data(#{data := Value}) ->
  Value.

-spec params(changeset()) -> params().
params(#{params := Value}) ->
  Value.

-spec errors(changeset()) -> errors().
errors(#{errors := Value}) ->
  Value.

-spec changes(changeset()) -> kv_map().
changes(#{changes := Value}) ->
  Value.

-spec is_valid(changeset()) -> boolean().
is_valid(#{is_valid := Value}) ->
  Value.

-spec types(changeset()) -> kv_map().
types(#{types := Value}) ->
  Value.

-spec required(changeset()) -> [atom()].
required(#{required := Value}) ->
  Value.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec add_error(changeset(), atom(), binary()) -> changeset().
add_error(Changeset, Key, Message) ->
  add_error(Changeset, Key, Message, []).

-spec add_error(changeset(), atom(), binary(), keyword()) -> changeset().
add_error(#{errors := Errors} = Changeset, Key, Message, Keys) ->
  Changeset#{errors := [{Key, {Message, Keys}} | Errors], is_valid := false}.

-spec apply_changes(changeset()) -> sumo:model().
apply_changes(#{changes := Changes, data := Data}) when map_size(Changes) == 0 ->
  Data;
apply_changes(#{changes := Changes, data := Data, types := Types}) ->
  maps:fold(fun(K, V, Acc) ->
    case maps:find(K, Types) of
      {ok, _} -> sumo_internal:set_field(K, V, Acc);
      error   -> Acc
    end
  end, Data, Changes).

-spec cast(changeset(), params(), [atom()]) -> changeset().
cast(#{schema := Schema, store := Store, data := Data, types := Types} = CS, Params, Allowed) ->
  NewChangeset = do_cast({Schema, Store, Data, Types}, Params, Allowed),
  cast_merge(CS, NewChangeset).

-spec cast(sumo:schema_name(), sumo:user_doc(), params(), [atom()]) -> changeset().
cast(SchemaName, UserDoc, Params, Allowed) ->
  Metadata = get_metadata(sumo_internal:from_user_doc(SchemaName, UserDoc)),
  do_cast(Metadata, Params, Allowed).

%% @private
do_cast({SchemaName, Store, Data, Types}, Params, Allowed) ->
  NewParams = convert_params(Params),
  FilteredParams = maps:with(Allowed, NewParams),

  {Changes, Errors, IsValid} = maps:fold(fun(ParamKey, ParamVal, Acc) ->
    process_param(ParamKey, ParamVal, Types, Acc)
  end, {#{}, [], true}, FilteredParams),

  (changeset())#{
    schema   := SchemaName,
    store    := Store,
    data     := Data,
    params   := FilteredParams,
    changes  := Changes,
    errors   := Errors,
    is_valid := IsValid,
    types    := Types
  }.

-spec change(changeset(), kv_map()) -> changeset().
change(#{changes := _, types := _} = Changeset, Changes) ->
  NewChanges = changes(get_changed(Changeset, Changes)),
  Changeset#{changes  := NewChanges}.

-spec change(sumo:schema_name(), sumo:user_doc(), kv_map()) -> changeset().
change(SchemaName, UserDoc, Changes) ->
  Doc = sumo_internal:from_user_doc(SchemaName, UserDoc),
  {SchemaName, Store, Data, Types} = get_metadata(Doc),
  Changeset = (changeset())#{
    schema   := SchemaName,
    store    := Store,
    data     := Data,
    types    := Types
  },
  NewChanges = changes(get_changed(Changeset, Changes)),
  Changeset#{changes  := NewChanges}.

get_changed(Changeset, NewChanges) ->
  maps:fold(fun(K, V, Acc) ->
    put_change(Acc, K, V)
  end, Changeset, NewChanges).

-spec get_field(changeset(), atom()) -> term().
get_field(Changeset, Key) ->
  get_field(Changeset, Key, undefined).

-spec get_field(changeset(), atom(), term()) -> term().
get_field(#{changes := Changes, data := Data}, Key, Default) ->
  case maps:find(Key, Changes) of
    {ok, Value} ->
      Value;
    error ->
      case maps:find(Key, Data) of
        {ok, Value} -> Value;
        error       -> Default
      end
  end.

-spec put_change(changeset(), atom(), term()) -> changeset().
put_change(#{changes := Changes, data := Data} = Changeset, Key, Value) ->
  NewChanges = case maps:find(Key, Data) of
    {ok, V} when V /= Value ->
      maps:put(Key, Value, Changes);
    _ ->
      case maps:is_key(Key, Changes) of
        true  -> maps:remove(Key, Changes);
        false -> Changes
      end
  end,
  Changeset#{changes := NewChanges}.

-spec get_change(changeset(), atom()) -> term().
get_change(Changeset, Key) ->
  get_change(Changeset, Key, undefined).

-spec get_change(changeset(), atom(), term()) -> term().
get_change(#{changes := Changes}, Key, Default) ->
  maps:get(Key, Changes, Default).

-spec delete_change(changeset(), atom()) -> changeset().
delete_change(#{changes := Changes} = Changeset, Key) ->
  NewChanges = maps:remove(Key, Changes),
  Changeset#{changes := NewChanges}.

-spec validate_change(changeset(), atom(), fun((atom(), term()) -> [error()])) -> changeset().
validate_change(#{changes := Changes, errors := Errors} = Changeset, Field, Validator) ->
  _ = ensure_field_exists(Changeset, Field),

  Value = fetch(Field, Changes),
  NewErrors1 = case is_nil(Value) of
    true  -> [];
    false -> Validator(Field, Value)
  end,
  NewErrors2 = [begin
    case Error of
      {K, V} when is_atom(K), is_binary(V) ->
        {K, {V, []}};
      {K, {V, Opts}} when is_atom(K), is_binary(V), is_list(Opts) ->
        {K, {V, Opts}}
    end
  end || Error <- NewErrors1],

  case NewErrors2 of
    []      -> Changeset;
    [_ | _] -> Changeset#{errors := NewErrors2 ++ Errors, is_valid := false}
  end.

-spec validate_required(changeset(), [atom()]) -> changeset().
validate_required(#{required := Required, errors := Errors} = CS, Fields) ->
  NewErrors = [begin
    {F, {<<"can't be blank">>, [{validation, required}]}}
  end || F <- Fields, is_missing(CS, F), ensure_field_exists(CS, F), is_nil(fetch(F, Errors))],

  case NewErrors of
    [] -> CS#{required := Fields ++ Required};
    _  -> CS#{required := Fields ++ Required, errors := NewErrors ++ Errors, is_valid := false}
  end.

-spec validate_inclusion(changeset(), atom(), [term()]) -> changeset().
validate_inclusion(Changeset, Field, Enum) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case lists:member(Value, Enum) of
      true  -> [];
      false -> [{Field, {<<"is invalid">>, [{validation, inclusion}]}}]
    end
  end).

-spec validate_number(changeset(), atom(), keyword()) -> changeset().
validate_number(Changeset, Field, Opts) ->
  validate_change(Changeset, Field, fun(TargetField, Value) ->
    hd([begin
      case maps:find(SpecKey, number_validators(TargetValue)) of
        {ok, {SpecFun, Message}} ->
          validate_number(TargetField, Value, Message, SpecFun, TargetValue);
        error ->
          error({badarg, SpecKey})
      end
    end || {SpecKey, TargetValue} <- Opts])
  end).

%% @private
validate_number(Field, Value, Message, SpecFun, TargetValue) ->
  case SpecFun(Value, TargetValue) of
    true  -> [];
    false -> [{Field, {Message, [{validation, number}]}}]
  end.

-spec validate_length(changeset(), atom(), keyword()) -> changeset().
validate_length(Changeset, Field, Opts) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case do_validate_length(length_validators(), Opts, byte_size(Value), undefined) of
      undefined -> [];
      Message   -> [{Field, {Message, [{validation, length}]}}]
    end
  end).

%% @private
do_validate_length([], _, _, Acc) ->
  Acc;
do_validate_length([{Opt, Validator} | T], Opts, Length, Acc) ->
  case fetch(Opt, Opts) of
    undefined ->
      do_validate_length(T, Opts, Length, Acc);
    Value ->
      case Validator(Length, Value) of
        undefined ->
          do_validate_length(T, Opts, Length, Acc);
        Message ->
          Message
      end
  end.

%% @private
wrong_length(Value, Value) ->
  undefined;
wrong_length(_Length, Value) ->
  sumo_utils:text("should be ~p character(s)", [Value]).

%% @private
too_short(Length, Value) when Length >= Value ->
  undefined;
too_short(_Length, Value) ->
  sumo_utils:text("should be at least ~p character(s)", [Value]).

%% @private
too_long(Length, Value) when Length =< Value ->
  undefined;
too_long(_Length, Value) ->
  sumo_utils:text("should be at most ~p character(s)", [Value]).

-spec validate_format(changeset(), atom(), binary()) -> changeset().
validate_format(Changeset, Field, Format) ->
  validate_change(Changeset, Field, fun(_, Value) ->
    case re:run(Value, Format) of
      nomatch -> [{Field, {<<"has invalid format">>, [{validation, format}]}}];
      _       -> []
    end
  end).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
changeset() ->
  #{schema   => undefined,
    store    => undefined,
    data     => undefined,
    params   => undefined,
    errors   => [],
    changes  => #{},
    is_valid => true,
    types    => undefined,
    required => []}.

%% @private
get_metadata(Doc) ->
  SchemaName = sumo_internal:doc_name(Doc),
  Store = sumo_config:get_store(SchemaName),
  Data = sumo_internal:doc_fields(Doc),
  Module = sumo_internal:doc_module(Doc),
  Types = schema_types(Module:sumo_schema()),
  {SchemaName, Store, Data, Types}.

%% @private
schema_types(Schema) ->
  lists:foldl(fun(F, Acc) ->
    maps:put(sumo_internal:field_name(F), sumo_internal:field_type(F), Acc)
  end, #{}, sumo_internal:schema_fields(Schema)).

%% @private
convert_params(Params) ->
  maps:fold(fun
    (K, V, Acc) when is_binary(K) -> maps:put(sumo_utils:to_atom(K), V, Acc);
    (K, _, Acc) when is_atom(K)   -> Acc
  end, Params, Params).

%% @private
process_param(ParamKey, ParamValue, Types, {Changes, Errors, IsValid}) ->
  Key = sumo_utils:to_atom(ParamKey),
  case cast_field(Key, ParamValue, Types) of
    {ok, CastValue} ->
      {maps:put(Key, CastValue, Changes), Errors, IsValid};
    {invalid, Type} ->
      {Changes, [{Key, {<<"is invalid">>, [{type, Type}, {validation, cast}]}} | Errors], false};
    missing ->
      {Changes, Errors, IsValid}
  end.

%% @private
cast_field(Key, Value, Types) ->
  case maps:get(Key, Types, error) of
    error ->
      missing;
    Type ->
      case sumo_type:cast(Type, Value) of
        {ok, _} = Ok -> Ok;
        {error, _}   -> {invalid, Type}
      end
  end.

%% @private
cast_merge(CS1, CS2) ->
  NewChanges = maps:merge(changes(CS1), changes(CS2)),
  NewErrors = lists:usort(errors(CS1) ++ errors(CS2)),
  NewIsValid = is_valid(CS1) and is_valid(CS2),
  NewTypes = types(CS1),
  NewRequired = lists:usort(required(CS1) ++ required(CS2)),
  NewParams = maps:merge(cs_params(CS1), cs_params(CS2)),

  CS1#{
    params   := NewParams,
    changes  := NewChanges,
    errors   := NewErrors,
    is_valid := NewIsValid,
    types    := NewTypes,
    required := NewRequired
  }.

%% @private
cs_params(#{params := Params}) ->
  case Params of
    undefined -> #{};
    _         -> Params
  end.

%% @private
is_missing(Changeset, Field) ->
  case get_field(Changeset, Field) of
    undefined -> true;
    _         -> false
  end.

%% @private
ensure_field_exists(#{types := Types}, Field) ->
  case maps:is_key(Field, Types) of
    true  -> true;
    false -> error({badarg, Field})
  end.

%% @private
is_nil(undefined) -> true;
is_nil(_)         -> false.

%% @private
fetch(Key, Keyword) when is_list(Keyword) ->
  sumo_utils:keyfind(Key, Keyword);
fetch(Key, Map) when is_map(Map) ->
  maps:get(Key, Map, undefined).

%% @private
number_validators(N) ->
  Text = fun(T) -> sumo_utils:text("must be ~s ~p", [T, N]) end,
  #{less_than                => {fun(X, Y) -> X < Y end,  Text("less than")},
    greater_than             => {fun(X, Y) -> X > Y end,  Text("greater than")},
    less_than_or_equal_to    => {fun(X, Y) -> X =< Y end, Text("less than or equal to")},
    greater_than_or_equal_to => {fun(X, Y) -> X >= Y end, Text("greater than or equal to")},
    equal_to                 => {fun(X, Y) -> X == Y end, Text("equal to")}}.

%% @private
length_validators() ->
  [{is, fun wrong_length/2}, {min, fun too_short/2}, {max, fun too_long/2}].
