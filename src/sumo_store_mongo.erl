%%% @hidden
%%% @doc MongoDB store implementation.
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
-module(sumo_store_mongo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behavior(sumo_store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([
  init/1, create_schema/2, persist/2, find_by/3, find_by/5, find_by/6,
  find_all/2,  find_all/5, delete_by/3, delete_all/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {pool:: pid()}).

-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, term()}.
init(Options) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Options),
  Pool    = sumo_backend_mongo:get_pool(Backend),
  {ok, #state{pool=Pool}}.

-spec persist(sumo_internal:doc(), state()) ->
  sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, #state{pool=Pool}=State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  NewId = case sumo_internal:get_field(IdField, Doc) of
    undefined -> emongo:oid();
    Id -> emongo:hex2dec(Id)
  end,
  Selector = [{"_id", {oid, NewId}}],
  NewDoc = sumo_internal:set_field(
    '_id',
    {oid, NewId},
    sumo_internal:set_field(IdField, emongo:dec2hex(NewId), Doc)
  ),
  NewDoc2 = sleep(NewDoc),
  Fields = sumo_internal:doc_fields(NewDoc2),
  ok = emongo:update(
    Pool, atom_to_list(DocName), Selector,
    maps:to_list(Fields), true
  ),
  {ok, NewDoc, State}.

-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, #state{pool = Pool} = State) ->
  % @TODO: Fix this workaround (it affects performance).
  % This is a workaround to get the number of docs to be deleted,
  % before to delete them, since the mongodb client `emongo'
  % doesn't return this value needed by sumo.
  % Besides, its is extremely recommended change the mongo client,
  % because the current one is too old, with lack of maintenance.
  TotalDocsToBeDeleted = length(emongo:find(
    Pool,
    atom_to_list(DocName),
    build_query(Conditions),
    [])),

  ok = emongo:delete(Pool,
                     atom_to_list(DocName),
                     build_query(Conditions)),
  {ok, TotalDocsToBeDeleted, State}.


-spec delete_all(sumo:schema_name(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(DocName, #state{pool=Pool}=State) ->
  ok = emongo:delete(Pool, atom_to_list(DocName)),
  {ok, unknown, State}.

-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              term(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName,
        Conditions,
        SortFields,
        Limit,
        Offset,
        #state{pool = Pool} = State
       ) ->
  Options = case Offset of
    0 -> [];
    Offset -> [{limit, Limit}, {offset, Offset}]
  end,

  Options1 = case SortFields of
               [] -> Options;
               _  -> [{orderby, SortFields} | Options]
             end,

  Query = build_query(translate_conditions(DocName, Conditions)),
  Results = emongo:find(Pool,
                        atom_to_list(DocName),
                        Query,
                        Options1),

  FoldFun =
    fun
      ({<<"_id">>, _FieldValue}, Acc) ->
        Acc;
      ({FieldName, FieldValue}, Acc) ->
        case is_binary(FieldValue) of
          true ->
            sumo_internal:set_field(
              list_to_atom(binary_to_list(FieldName)),
              binary_to_list(FieldValue),
              Acc
             );
          false ->
            sumo_internal:set_field(
              list_to_atom(binary_to_list(FieldName)),
              FieldValue,
              Acc
             )
        end
    end,

  Docs =
    lists:map(
      fun(Row) ->
        wakeup(lists:foldl(
          FoldFun,
          sumo_internal:new_doc(DocName),
          Row
         ))
      end,
      Results
     ),

  {ok, Docs, State}.

-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

-spec find_all(sumo:schema_name(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

-spec find_all(sumo:schema_name(),
               term(),
               non_neg_integer(),
               non_neg_integer(),
               state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, SortFields, Limit, Offset, State) ->
  %% If conditions is empty then no documents are returned.
  Conditions = [{'_id', not_null}],
  find_by(DocName, Conditions, SortFields, Limit, Offset, State).

-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(Schema, #state{pool=Pool} = State) ->
  SchemaName = sumo_internal:schema_name(Schema),
  Fields = sumo_internal:schema_fields(Schema),
  lists:foreach(
    fun(Field) ->
      create_field(Field),
      Name = sumo_internal:field_name(Field),
      ok = emongo:ensure_index(
        Pool, atom_to_list(SchemaName), [{atom_to_list(Name), 1}])
    end,
    Fields
  ),
  {ok, State}.

create_field(Field) ->
  Attrs = sumo_internal:field_attrs(Field),
  lists:foldl(
    fun(Attr, Acc) ->
      case create_index(Attr) of
        none -> Acc;
        IndexProp -> [IndexProp|Acc]
      end
    end,
    [],
    Attrs
  ).

create_index(index) ->
  none;

create_index(unique) ->
  {unique, 1};

create_index(id) ->
  {unique, 1};

create_index(_Attr) ->
  none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec build_query(sumo:conditions()) -> iodata().
build_query(Exprs) when is_list(Exprs) ->
  lists:flatmap(fun build_query/1, Exprs);
build_query({'and', Exprs}) ->
  WrappedExpr = [[Expr] || Expr <- build_query(Exprs)],
  [{<<"$and">>, {array, WrappedExpr}}];
build_query({'or', Exprs}) ->
  WrappedExpr = [[Expr] || Expr <- build_query(Exprs)],
  [{<<"$or">>, {array, WrappedExpr}}];
build_query({'not', Expr}) ->
  [{<<"$not">>, build_query(Expr)}];

build_query({_Name1, _Op, Name2} = Expr) when is_atom(Name2) ->
  throw({unsupported_expression, Expr});
build_query({Name, '/=', Value}) ->
  [{Name, [{<<"$ne">>, Value}]}];
build_query({Name, '==', Value}) ->
  [{Name, Value}];
build_query({Name, '=<', Value}) ->
  [{Name, [{<<"$lte">>, Value}]}];
build_query({Name, '>=', Value}) ->
  [{Name, [{<<"$gte">>, Value}]}];
build_query({Name, '<', Value}) ->
  [{Name, [{<<"$lt">>, Value}]}];
build_query({Name, '>', Value}) ->
  [{Name, [{<<"$gt">>, Value}]}];
build_query({Name, 'like', Value}) ->
  Regex = like_to_regex(Value),
  [{Name, {regexp, Regex, "i"}}];
build_query({_, Op, _})  ->
  sumo_internal:check_operator(Op);

build_query({Name, 'null'}) ->
  [{Name, undefined}];
build_query({Name, 'not_null'}) ->
  [{Name, [{<<"$ne">>, undefined}]}];
build_query({Name, Value}) ->
  [{Name, Value}].

like_to_regex(Like) ->
  Bin = list_to_binary(Like),
  Regex0 = binary:replace(Bin, <<"%">>, <<".*">>, [global]),
  Regex1 = case hd(Like) of
             $% -> Regex0;
             _ -> <<"^", Regex0/binary>>
           end,
  case lists:last(Like) of
    $% -> Regex1;
    _ -> <<Regex1/binary, "$">>
  end.

%% @private
sleep(Doc) ->
  DTFields = datetime_fields_from_doc(Doc),
  lists:foldl(fun({FieldName, FieldType, FieldValue}, Acc) ->
    case {FieldType, is_datetime(FieldValue)} of
      {date, true} ->
        sumo_internal:set_field(FieldName, {FieldValue, {0, 0, 0}}, Acc);
      _ ->
        Acc
    end
  end, Doc, DTFields).

%% @private
wakeup(Doc) ->
  Fields = sumo_utils:fields_from_doc(Doc),
  lists:foldl(fun({FieldName, FieldType, FieldValue}, Acc) ->
    case {FieldType, FieldValue} of
      {datetime, {_, _, _}} ->
        DateTime = calendar:now_to_universal_time(FieldValue),
        sumo_internal:set_field(FieldName, DateTime, Acc);
      {date, {_, _, _}} ->
        {Date, _} = calendar:now_to_universal_time(FieldValue),
        sumo_internal:set_field(FieldName, Date, Acc);
      {integer, FieldValue} ->
        Integer = sumo_utils:to_int(FieldValue),
        sumo_internal:set_field(FieldName, Integer, Acc);
      {binary, FieldValue} ->
        Binary = sumo_utils:to_bin(FieldValue),
        sumo_internal:set_field(FieldName, Binary, Acc);
      {text, FieldValue} ->
        Text = sumo_utils:to_bin(FieldValue),
        sumo_internal:set_field(FieldName, Text, Acc);
      {float, FieldValue} ->
        Float = sumo_utils:to_float(FieldValue),
        sumo_internal:set_field(FieldName, Float, Acc);
      {string, FieldValue} ->
        String = sumo_utils:to_list(FieldValue),
        sumo_internal:set_field(FieldName, String, Acc);
      _ ->
        Acc
    end
  end, Doc, Fields).

%% @private
datetime_fields_from_doc(Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  lists:foldl(fun(Field, Acc) ->
    FieldType = sumo_internal:field_type(Field),
    case FieldType of
      T when T =:= datetime; T =:= date ->
        FieldName = sumo_internal:field_name(Field),
        FieldValue = sumo_internal:get_field(FieldName, Doc),
        [{FieldName, FieldType, FieldValue} | Acc];
     _ ->
        Acc
    end
  end, [], SchemaFields).

%% @private
datetime_fields_from_docname(DocName) ->
  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  lists:foldl(fun(Field, Acc) ->
    FieldType = sumo_internal:field_type(Field),
    case FieldType of
      _ when FieldType =:= datetime; FieldType =:= date ->
        FieldName = sumo_internal:field_name(Field),
        [{FieldName, FieldType} | Acc];
      _ ->
        Acc
    end
  end, [], SchemaFields).

%% @private
is_datetime({{_, _, _} = Date, {_, _, _}}) ->
  calendar:valid_date(Date);
is_datetime({_, _, _} = Date) ->
  calendar:valid_date(Date);
is_datetime(_) ->
  false.

%% @private
translate_conditions(DocName, Conditions) when is_list(Conditions) ->
  DTFields = datetime_fields_from_docname(DocName),
  lists:foldl(fun
    ({K, V}, Acc) when K == 'and'; K == 'or' ->
      [{K, translate_conditions(DocName, V)} | Acc];
    ({'not', V}, Acc) ->
      [NotCond] = translate_conditions(DocName, [V]),
      [{'not', NotCond} | Acc];
    ({K, V} = KV, Acc) ->
      case lists:keyfind(K, 1, DTFields) of
        {K, FieldType} ->
          [{K, validate_date(V, FieldType)} | Acc];
        false ->
          [KV | Acc]
      end;
    ({K, Op, V} = KV, Acc) ->
      case lists:keyfind(K, 1, DTFields) of
        {K, FieldType} ->
          [{K, Op, validate_date(V, FieldType)} | Acc];
        false ->
          [KV | Acc]
      end;
    (Cond, Acc) ->
      [Cond | Acc]
  end, [], Conditions);
translate_conditions(DocName, Conditions) ->
  translate_conditions(DocName, [Conditions]).

%% @private
validate_date(FieldValue, FieldType) ->
  case {FieldType, is_datetime(FieldValue)} of
    {datetime, true} -> FieldValue;
    {date, true}     -> {FieldValue, {0, 0, 0}}
  end.
