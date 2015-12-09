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
  Fields = sumo_internal:doc_fields(NewDoc),
  ok = emongo:update(
    Pool, atom_to_list(DocName), Selector,
    maps:to_list(Fields), true
  ),
  {ok, NewDoc, State}.

-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, #state{pool = Pool} = State) ->
  ok = emongo:delete(Pool,
                     atom_to_list(DocName),
                     build_query(Conditions)),
  {ok, 1, State}.


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

  Results = emongo:find(Pool,
                        atom_to_list(DocName),
                        build_query(Conditions),
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
          lists:foldl(
            FoldFun,
            sumo_internal:new_doc(DocName),
            Row
           )
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
