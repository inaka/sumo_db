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
  init/1, create_schema/2, persist/2, find_by/3, find_by/5,
  delete/3, delete_by/3, delete_all/2
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

find_by(DocName, Conditions, Limit, Offset, #state{pool=Pool}=State) ->
  Options = case Offset of
    0 -> [];
    Offset -> [{limit, Limit}, {offset, Offset}]
  end,
  Docs = lists:reverse(lists:map(
    fun(Row) ->
      lists:foldl(
        fun({FieldName, FieldValue}, Acc) ->
          case FieldName of
            <<"_id">> -> Acc;
            _ -> if
              is_binary(FieldValue) ->
                sumo_internal:set_field(
                  list_to_atom(binary_to_list(FieldName)),
                  binary_to_list(FieldValue),
                  Acc
                );
              true ->
                sumo_internal:set_field(
                  list_to_atom(binary_to_list(FieldName)),
                  FieldValue,
                  Acc
                )
            end
          end
        end,
        sumo_internal:new_doc(DocName, []),
        Row
      )
    end,
    emongo:find(Pool, atom_to_list(DocName), Conditions, Options)
  )),
  {ok, Docs, State}.

find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

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
