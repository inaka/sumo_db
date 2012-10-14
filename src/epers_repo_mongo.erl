%%% @doc MongoDB repository implementation.
%%%
%%% Copyright 2012 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
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
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(epers_repo_mongo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include_lib("include/epers_doc.hrl").

-behavior(epers_repo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([
  init/1, create_schema/2, persist/2, find_by/3, find_by/5,
  delete/3, delete_all/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {pool}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
persist(#epers_doc{name=DocName}=Doc, #state{pool=Pool}=State) ->
  IdField = epers:field_name(epers:get_id_field(DocName)),
  NewId = case epers:get_field(IdField, Doc) of
    undefined -> emongo:oid();
    Id -> emongo:hex2dec(Id)
  end,
  Selector = [{"_id", {oid, NewId}}],
  NewDoc = epers:set_field(
    '_id', {oid, NewId}, epers:set_field(IdField, emongo:dec2hex(NewId), Doc)
  ),
  ok = emongo:update(
    Pool, atom_to_list(DocName), Selector, NewDoc#epers_doc.fields, true
  ),
  {ok, NewDoc, State}.

delete(DocName, Id, #state{pool=Pool}=State) ->
  IdField = epers:field_name(epers:get_id_field(DocName)),
  ok = emongo:delete(
    Pool, atom_to_list(DocName), [{atom_to_list(IdField), Id}]
  ),
  {ok, 1, State}.

delete_all(DocName, #state{pool=Pool}=State) ->
  lager:debug("Dropping collection: ~p", [DocName]),
  ok = emongo:delete(Pool, atom_to_list(DocName)),
  {ok, State}.

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
                epers:set_field(
                  list_to_atom(binary_to_list(FieldName)),
                  binary_to_list(FieldValue),
                  Acc
                );
              true ->
                epers:set_field(
                  list_to_atom(binary_to_list(FieldName)),
                  FieldValue,
                  Acc
                )
            end
          end
        end,
        epers:new_doc(DocName),
        Row
      )
    end,
    emongo:find(Pool, atom_to_list(DocName), Conditions, Options)
  )),
  {ok, Docs, State}.

find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

create_schema(
  #epers_schema{name=SchemaName, fields=Fields}, #state{pool=Pool}=State
) ->
  lists:foreach(
    fun(#epers_field{name=Name, attrs=Attrs}) ->
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
      lager:debug("Creating index: ~p for ~p", [Name, SchemaName]),
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
  Pool = erlang:ref_to_list(make_ref()),
  ok = emongo:add_pool(
    Pool,
    proplists:get_value(host, Options, "localhost"),
    proplists:get_value(port, Options, 27017),
    proplists:get_value(database, Options),
    1
  ),
  emongo:auth(
    Pool,
    proplists:get_value(username, Options),
    proplists:get_value(password, Options)
  ),
  {ok, #state{pool=Pool}}.
