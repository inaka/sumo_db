%%% @hidden
%%% @doc MongoDB repository implementation.
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
-module(sumo_repo_mongo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behavior(sumo_repo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([
  init/1, create_schema/2, persist/2, find_by/3, find_by/5, find_by/6,
  find_all/2,  find_all/5, delete/3, delete_by/3, delete_all/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {pool:: pid()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
persist(Doc, #state{pool=Pool}=State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  NewId = case sumo_internal:get_field(IdField, Doc) of
    undefined -> emongo:oid();
    Id -> emongo:hex2dec(Id)
  end,
  Selector = [{"_id", {oid, NewId}}],
  NewDoc = sumo_internal:set_field(
    '_id', {oid, NewId}, sumo_internal:set_field(IdField, emongo:dec2hex(NewId), Doc)
  ),
  ok = emongo:update(
    Pool, atom_to_list(DocName), Selector,
    sumo_internal:doc_fields(NewDoc), true
  ),
  {ok, NewDoc, State}.

delete(DocName, Id, #state{pool=Pool}=State) ->
  IdField = sumo_internal:id_field_name(DocName),
  ok = emongo:delete(
    Pool, atom_to_list(DocName), [{atom_to_list(IdField), Id}]
  ),
  {ok, 1, State}.

delete_by(DocName, Conditions, State) ->
  Args = [?MODULE, DocName, Conditions, State],
  lager:critical("Unimplemented function: ~p:delete_by(~p, ~p, ~p)", Args),
  {error, not_implemented, State}.

delete_all(DocName, #state{pool=Pool}=State) ->
  lager:debug("dropping collection: ~p", [DocName]),
  ok = emongo:delete(Pool, atom_to_list(DocName)),
  {ok, unknown, State}.

find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

find_by(
  DocName, Conditions, SortFields, Limit, Offset, #state{pool = Pool} = State
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
            sumo_internal:new_doc(DocName, []),
            Row
           )
      end,
      Results
     ),

  {ok, Docs, State}.

find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

find_all(DocName, SortFields, Limit, Offset, State) ->
  %% If conditions is empty then no documents are returned.
  Conditions = [{'_id', not_null}],
  find_by(DocName, Conditions, SortFields, Limit, Offset, State).

create_schema(Schema, #state{pool=Pool}=State) ->
  SchemaName = sumo_internal:schema_name(Schema),
  Fields = sumo_internal:schema_fields(Schema),
  lists:foreach(
    fun(Field) ->
      Name = sumo_internal:field_name(Field),
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
      ),
      lager:debug("creating index: ~p for ~p", [Name, SchemaName]),
      ok = emongo:ensure_index(
        Pool, atom_to_list(SchemaName), [{atom_to_list(Name), 1}]
      )
    end,
    Fields
  ),
  {ok, State}.

create_index(index) ->
  none;

create_index(unique) ->
  {unique, 1};

create_index(id) ->
  {unique, 1};

create_index(_Attr) ->
  none.

init(Options) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Options),
  Pool    = sumo_backend_mysql:get_pool(Backend),
  {ok, #state{pool=Pool}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec build_query(sumo_internal:expression()) -> iodata().
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
