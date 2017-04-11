%%% @hidden
%%% @doc PostgreSql store implementation.
%%%
%%% Copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sumo_store_mnesia).
-author("Brujo Benavides <elbrujohalcon@inaka.net>").
-license("Apache License 2.0").

-behaviour(sumo_store).

%% API
-export([
  init/1,
  create_schema/2,
  persist/2,
  fetch/3,
  delete_by/3,
  delete_all/2,
  find_all/2, find_all/5,
  find_by/3, find_by/5, find_by/6,
  count/2
]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type option() :: disc_copies | ram_copies | majority
                | snmp | storage_properties.

-type state() :: #{
  verbose         => boolean(),
  default_options => [{option(), term()}]
}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(term()) -> {ok, state()}.
init(Options) ->
  DefaultOptions = parse(Options),
  Verbose = application:get_env(sumo_db, verbose, false),
  {ok, #{default_options => DefaultOptions, verbose => Verbose}}.

-spec persist(Doc, State) -> Response when
  Doc      :: sumo_internal:doc(),
  State    :: state(),
  Response :: sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = sumo_internal:get_field(IdField, Doc),
  NewId = case Id of
    undefined -> new_id(DocName, sumo_internal:id_field_type(DocName));
    Id        -> Id
  end,

  Doc2 = sleep(Doc),
  Fields = sumo_internal:doc_fields(Doc2),
  Schema = sumo_internal:get_schema(DocName),
  [IdField | NPFields] = schema_field_names(Schema),
  NPValues = [maps:get(K, Fields, undefined) || K <- NPFields],
  MnesiaRecord = list_to_tuple([DocName, NewId | NPValues]),

  case mnesia:transaction(fun() -> mnesia:write(MnesiaRecord) end) of
    {aborted, Reason} ->
      {error, Reason, State};
    {atomic, ok} ->
      NewDoc = sumo_internal:set_field(IdField, NewId, Doc),
      _ = maybe_log(persist, [DocName, NewDoc], State),
      {ok, NewDoc, State}
  end.

-spec fetch(DocName, Id, State) -> Response when
  DocName  :: sumo:schema_name(),
  Id       :: sumo:field_value(),
  State    :: state(),
  Response :: sumo_store:result(sumo_internal:doc(), state()).
fetch(DocName, Id, State) ->
  try
    [Result] = mnesia:dirty_read(DocName, Id),
    Schema = sumo_internal:get_schema(DocName),
    Fields = schema_field_names(Schema),
    _ = maybe_log(fetch, [DocName, Id], State),
    {ok, wakeup(result_to_doc(Result, Fields)), State}
  catch
    _:_ -> {error, notfound, State}
  end.

-spec delete_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, State) ->
  MatchSpec = build_match_spec(DocName, Conditions),
  Transaction = fun() ->
    Items = mnesia:select(DocName, MatchSpec),
    lists:foreach(fun mnesia:delete_object/1, Items),
    length(Items)
  end,
  case mnesia:transaction(Transaction) of
    {aborted, Reason} ->
      {error, Reason, State};
    {atomic, Result} ->
      _ = maybe_log(delete_by, [DocName, Conditions], State),
      {ok, Result, State}
  end.

-spec delete_all(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(DocName, State) ->
  Count = mnesia:table_info(DocName, size),
  case mnesia:clear_table(DocName) of
    {atomic, ok} ->
      _ = maybe_log(delete_all, [DocName], State),
      {ok, Count, State};
    {aborted, Reason} ->
      {error, Reason, State}
  end.

-spec find_all(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

-spec find_all(DocName, SortFields, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  SortFields :: term(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, SortFields, Limit, Offset, State) ->
  find_by(DocName, [], SortFields, Limit, Offset, State).

-spec find_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, [], 0, 0, State).

-spec find_by(DocName, Conditions, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

-spec find_by(DocName, Conditions, Sort, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Sort       :: term(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, [], Limit, Offset, State) ->
  MatchSpec = build_match_spec(DocName, Conditions),
  Transaction0 = fun() ->
    mnesia:select(DocName, MatchSpec)
  end,
  TransactionL = fun() ->
    case mnesia:select(DocName, MatchSpec, Offset + Limit, read) of
      {ManyItems, _Cont} ->
        lists:sublist(ManyItems, Offset + 1, Limit);
      '$end_of_table' ->
        []
    end
  end,
  Transaction = case Limit of
    0     -> Transaction0;
    Limit -> TransactionL
  end,
  case mnesia:transaction(Transaction) of
    {aborted, Reason} ->
      {error, Reason, State};
    {atomic, Results} ->
      Schema = sumo_internal:get_schema(DocName),
      Fields = schema_field_names(Schema),
      Docs = [wakeup(result_to_doc(Result, Fields)) || Result <- Results],
      _ = maybe_log(find_by, [DocName, Conditions, Limit, Offset, MatchSpec], State),
      {ok, Docs, State}
  end;
find_by(_DocName, _Conditions, _Sort, _Limit, _Offset, State) ->
  {error, not_supported, State}.

-spec create_schema(Schema, State) -> Response when
  Schema   :: sumo:schema(),
  State    :: state(),
  Response :: sumo_store:result(state()).
create_schema(Schema, #{default_options := DefaultOptions} = State) ->
  Name = sumo_internal:schema_name(Schema),
  Fields = schema_fields(Schema),
  Attributes = [sumo_internal:field_name(Field) || Field <- Fields],
  Indexes = [
    sumo_internal:field_name(Field)
    || Field <- Fields, lists:member(index, sumo_internal:field_attrs(Field))
  ],
  Options = [
    {attributes, Attributes},
    {index, Indexes}
    | DefaultOptions
  ],
  case mnesia:create_table(Name, Options) of
    {atomic, ok}                      -> {ok, State};
    {aborted, {already_exists, Name}} -> {ok, State};
    {aborted, Reason}                 -> {error, Reason, State}
  end.

-spec count(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result(non_neg_integer(), state()).
count(DocName, State) ->
  try
    Size = mnesia:table_info(DocName, size),
    {ok, Size, State}
  catch
    _:Reason -> {error, Reason, State}
  end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
parse(Options) -> parse(Options, []).

%% @private
parse([], Acc) ->
  Acc;
parse([{disc_copies, here} | Options], Acc) ->
  parse(Options, [{disc_copies, [node()]} | Acc]);
parse([{disc_copies, Nodes} | Options], Acc) ->
  parse(Options, [{disc_copies, Nodes} | Acc]);
parse([{disc_only_copies, here} | Options], Acc) ->
  parse(Options, [{disc_only_copies, [node()]} | Acc]);
parse([{disc_only_copies, Nodes} | Options], Acc) ->
  parse(Options, [{disc_only_copies, Nodes} | Acc]);
parse([{ram_copies, here} | Options], Acc) ->
  parse(Options, [{ram_copies, [node()]} | Acc]);
parse([{ram_copies, Nodes} | Options], Acc) ->
  parse(Options, [{ram_copies, Nodes} | Acc]);
parse([{majority, Flag} | Options], Acc) ->
  parse(Options, [{majority, Flag} | Acc]);
parse([{snmp, SnmpStruct} | Options], Acc) ->
  parse(Options, [{snmp, SnmpStruct} | Acc]);
parse([{storage_properties, Props} | Options], Acc) ->
  parse(Options, [{storage_properties, Props} | Acc]);
parse([_IgnoredOption | Options], Acc) ->
  parse(Options, Acc).

%% @private
new_id(DocName, FieldType) ->
  NewId = new_id(FieldType),
  case mnesia:dirty_read(DocName, NewId) of
    [] -> NewId;
    _  -> new_id(DocName, FieldType)
  end.

%% @private
new_id(string)    -> uuid:uuid_to_string(uuid:get_v4(), standard);
new_id(binary)    -> uuid:uuid_to_string(uuid:get_v4(), binary_standard);
new_id(integer)   -> <<Id:128>> = uuid:get_v4(), Id;
new_id(float)     -> <<Id:128>> = uuid:get_v4(), Id * 1.0;
new_id(FieldType) -> exit({unimplemented, FieldType}).

%% @doc http://www.erlang.org/doc/apps/erts/match_spec.html
%% @private
build_match_spec(DocName, Condition) when not is_list(Condition) ->
  build_match_spec(DocName, [Condition]);
build_match_spec(DocName, Conditions) ->
  NewConditions = transform_conditions(DocName, Conditions),
  Schema = sumo_internal:get_schema(DocName),
  Fields = schema_field_names(Schema),
  FieldsMap = maps:from_list(
    [field_tuple(I, Fields) || I <- lists:seq(1, length(Fields))]),
  % The following ordering function avoids '$10' been added between
  % '$1' and '$2' in the MatchHead list. Without this fix, this store
  % would fail when trying to use `find_by` function.
  OrderingFun = fun(A, B) ->
    "$" ++ ANumber = atom_to_list(A),
    "$" ++ BNumber = atom_to_list(B),
    list_to_integer(ANumber) =< list_to_integer(BNumber)
  end,
  ValuesSorted = lists:sort(OrderingFun, maps:values(FieldsMap)),
  MatchHead = list_to_tuple([DocName | ValuesSorted]),
  Guard = [
    condition_to_guard(Condition, FieldsMap) || Condition <- NewConditions
  ],
  Result = '$_',
  [{MatchHead, Guard, [Result]}].

%% @private
field_tuple(I, Fields) ->
  FieldName = lists:nth(I, Fields),
  FieldWildcard = list_to_atom([$$ | integer_to_list(I)]),
  {FieldName, FieldWildcard}.

%% @private
condition_to_guard({'and', [Expr1]}, FieldsMap) ->
  condition_to_guard(Expr1, FieldsMap);
condition_to_guard({'and', [Expr1 | Exprs]}, FieldsMap) ->
  { 'andalso'
  , condition_to_guard(Expr1, FieldsMap)
  , condition_to_guard({'and', Exprs}, FieldsMap)
  };
condition_to_guard({'or', [Expr1]}, FieldsMap) ->
  condition_to_guard(Expr1, FieldsMap);
condition_to_guard({'or', [Expr1 | Exprs]}, FieldsMap) ->
  { 'orelse'
  , condition_to_guard(Expr1, FieldsMap)
  , condition_to_guard({'or', Exprs}, FieldsMap)
  };
condition_to_guard({'not', Expr}, FieldsMap) ->
  {'not', condition_to_guard(Expr, FieldsMap)};
condition_to_guard({Name1, Op, Name2}, FieldsMap) when is_atom(Name2) ->
  check_operator(Op),
  %NOTE: Name2 can be a field name or a value, that's why the following happens
  {Op, maps:get(Name1, FieldsMap), maps:get(Name2, FieldsMap, {const, Name2})};
condition_to_guard({Name1, Op, Value}, FieldsMap) ->
  check_operator(Op),
  {Op, maps:get(Name1, FieldsMap), {const, Value}};
condition_to_guard({Name, 'null'}, FieldsMap) ->
  condition_to_guard({Name, '==', undefined}, FieldsMap);
condition_to_guard({Name, 'not_null'}, FieldsMap) ->
  condition_to_guard({Name, '/=', undefined}, FieldsMap);
condition_to_guard({Name, Value}, FieldsMap) ->
  condition_to_guard({Name, '==', Value}, FieldsMap).

%% @private
check_operator(like) -> exit({unsupported_operator, like});
check_operator(Op)   -> sumo_internal:check_operator(Op).

%% @private
schema_field_names(Schema) ->
  [sumo_internal:field_name(Field) || Field <- schema_fields(Schema)].

%% @private
schema_fields(Schema) ->
  place_id_first(sumo_internal:schema_fields(Schema)).

%% @private
place_id_first(Fields) ->
  place_id_first(Fields, []).
place_id_first([], Acc) ->
  lists:reverse(Acc);
place_id_first([Field|Fields], Acc) ->
  case lists:member(id, sumo_internal:field_attrs(Field)) of
    true  -> [Field|lists:reverse(Acc)] ++ Fields;
    false -> place_id_first(Fields, [Field|Acc])
  end.

%% @private
result_to_doc(Result, Fields) ->
  [DocName | Values] = tuple_to_list(Result),
  NewDoc = sumo_internal:new_doc(DocName),
  Pairs = lists:zip(Fields, Values),
  lists:foldl(fun({Name, Value}, Doc) ->
    sumo_internal:set_field(Name, Value, Doc)
  end, NewDoc, Pairs).

%% @private
transform_conditions(DocName, Conditions) ->
  sumo_utils:transform_conditions(fun validate_date/1, DocName, Conditions, [date]).

%% @private
validate_date({FieldType, _, FieldValue}) ->
  case {FieldType, sumo_utils:is_datetime(FieldValue)} of
    {date, true} ->
      {FieldValue, {0, 0, 0}}
  end.

%% @private
sleep(Doc) ->
  sumo_utils:doc_transform(fun sleep_fun/4, Doc).

%% @private
sleep_fun(_, _, undefined, _) ->
  undefined;
sleep_fun(string, _, FieldValue, _) ->
  sumo_utils:to_bin(FieldValue);
sleep_fun(date, _, FieldValue, _) ->
  case sumo_utils:is_datetime(FieldValue) of
    true -> {FieldValue, {0, 0, 0}};
    _    -> FieldValue
  end;
sleep_fun(_, _, FieldValue, _) ->
  FieldValue.

%% @private
wakeup(Doc) ->
  sumo_utils:doc_transform(fun wakeup_fun/4, Doc).

%% @private
wakeup_fun(date, _, {Date, _} = _FieldValue, _) ->
  Date;
wakeup_fun(_, _, FieldValue, _) ->
  FieldValue.

%% @private
maybe_log(Fun, Args, #{verbose := true}) ->
  error_logger:info_msg(log_format(Fun), Args);
maybe_log(_, _, _) ->
  ok.

%% @private
log_format(persist)    -> "persist(~p, ~p)";
log_format(fetch)      -> "fetch(~p, ~p)";
log_format(delete_by)  -> "delete_by(~p, ~p)";
log_format(delete_all) -> "delete_all(~p)";
log_format(find_by)    -> "find_by(~p, ~p, [], ~p, ~p)~nMatchSpec: ~p".
