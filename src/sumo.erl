%%% @doc Main module for sumo. Use this one from your own applications.
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
-module(sumo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%%% API for doc/schema manipulation.
-export([
  new_schema/2,
  new_field/3,
  new_field/2
]).

%%% API for schema creation.
-export([
  create_schema/0,
  create_schema/1,
  create_schema/2
]).

%%% API for standard CRUD functions.
-export([
  persist/2,
  fetch/2,
  find_one/2,
  find_all/1, find_all/4,
  find_by/2, find_by/4, find_by/5,
  delete/2,
  delete_by/2,
  delete_all/1,
  count/1,
  call/2, call/3
]).

%%% Types
-type schema_name() :: atom().

-type custom_attr() :: term().

-type field_attr() :: id | unique | index | not_null | auto_increment
                    | {length, integer()} | custom_attr().

-type field_attrs() :: [field_attr()].

-type field_type() :: integer | float | string | binary | boolean
                    | date | datetime | custom.

-type field_name() :: atom().

-type field_value() :: term().

-type operator() :: '<' | '>' | '==' | '=<' | '>=' | '/=' | 'like'.

-type model() :: #{field_name() => field_value()}.

-type condition() :: {'and', [condition()]} | {'or', [condition()]}
                   | {'not', condition()} | {field_name(), field_value()}
                   | {field_name(), operator(), field_value()}
                   | {field_name(), operator(), field_name()}.

-type conditions() :: condition() | [condition()].

-type sort_order() :: asc | desc.

-type sort() :: field_name()
              | {field_name(), sort_order()}
              | [{field_name(), sort_order()}].

-type schema() :: sumo_internal:schema().

-type field() :: sumo_internal:field().

-type user_doc() :: term().

-export_type([
  schema_name/0,
  field_attr/0,
  field_attrs/0,
  field_type/0,
  field_name/0,
  field_value/0,
  model/0,
  conditions/0,
  sort/0,
  sort_order/0,
  operator/0,
  condition/0,
  schema/0,
  field/0,
  user_doc/0
]).

%%%=============================================================================
%%% Code starts here.
%%%=============================================================================

%% @doc Creates or updates the given Doc.
-spec persist(schema_name(), user_doc()) -> user_doc().
persist(DocName, UserDoc) ->
  Module = sumo_config:get_prop_value(DocName, module),
  DocMap = Module:sumo_sleep(UserDoc),
  Store = sumo_config:get_store(DocName),
  sumo_event:dispatch(DocName, pre_persisted, [UserDoc]),
  case sumo_store:persist(Store, sumo_internal:new_doc(DocName, DocMap)) of
    {ok, NewDoc} ->
      Ret = sumo_internal:wakeup(NewDoc),
      sumo_event:dispatch(DocName, persisted, [Ret]),
      Ret;
    Error ->
      throw(Error)
  end.

%% @doc Returns the doc identified by Id.
-spec fetch(schema_name(), field_value()) -> user_doc() | notfound.
fetch(DocName, Id) ->
  Store = sumo_config:get_store(DocName),
  case sumo_store:fetch(Store, DocName, Id) of
    {ok, Doc}       -> sumo_internal:wakeup(Doc);
    {error, Reason} -> Reason
  end.

%% @doc Returns 1 doc that matches the given Conditions.
-spec find_one(schema_name(), conditions()) -> user_doc() | notfound.
find_one(DocName, Conditions) ->
  case find_by(DocName, Conditions, 1, 0) of
    []          -> notfound;
    [First | _] -> First
  end.

%% @doc Returns all docs from the given store.
-spec find_all(schema_name()) -> [user_doc()].
find_all(DocName) ->
  case sumo_store:find_all(sumo_config:get_store(DocName), DocName) of
    {ok, Docs} -> docs_wakeup(Docs);
    Error      -> throw(Error)
  end.

%% @doc Returns Limit docs from the given store, starting at offset.
-spec find_all(DocName, SortFields0, Limit, Offset) -> Res when
  DocName     :: schema_name(),
  SortFields0 :: sort(),
  Limit       :: non_neg_integer(),
  Offset      :: non_neg_integer(),
  Res         :: [user_doc()].
find_all(DocName, SortFields0, Limit, Offset) ->
  SortFields = normalize_sort_fields(SortFields0),
  Store = sumo_config:get_store(DocName),
  case sumo_store:find_all(Store, DocName, SortFields, Limit, Offset) of
    {ok, Docs} -> docs_wakeup(Docs);
    Error      -> throw(Error)
  end.

%% @doc Returns *all* docs that match Conditions.
-spec find_by(schema_name(), conditions()) -> [user_doc()].
find_by(DocName, Conditions) ->
  Store = sumo_config:get_store(DocName),
  case sumo_store:find_by(Store, DocName, Conditions) of
    {ok, Docs} -> docs_wakeup(Docs);
    Error      -> throw(Error)
  end.

%% @doc
%% Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
%% @end
-spec find_by(DocName, Conditions, Limit, Offset) -> Res when
  DocName    :: schema_name(),
  Conditions :: conditions(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: [user_doc()].
find_by(DocName, Conditions, Limit, Offset) ->
  Store = sumo_config:get_store(DocName),
  case sumo_store:find_by(Store, DocName, Conditions, Limit, Offset) of
    {ok, Docs} -> docs_wakeup(Docs);
    Error      -> throw(Error)
  end.

%% @doc
%% Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
%% @end
-spec find_by(DocName, Conditions, SortFields, Limit, Offset) -> Res when
  DocName    :: schema_name(),
  Conditions :: conditions(),
  SortFields :: sort(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: [user_doc()].
find_by(DocName, Conditions, SortFields, Limit, Offset) ->
  NormalizedSortFields = normalize_sort_fields(SortFields),
  Store = sumo_config:get_store(DocName),
  case sumo_store:find_by(
    Store, DocName, Conditions, NormalizedSortFields, Limit, Offset
  ) of
    {ok, Docs} -> docs_wakeup(Docs);
    Error      -> throw(Error)
  end.

%% @doc Deletes all docs of type DocName.
-spec delete_all(schema_name()) -> non_neg_integer().
delete_all(DocName) ->
  Store = sumo_config:get_store(DocName),
  sumo_event:dispatch(DocName, pre_delete_all),
  case sumo_store:delete_all(Store, DocName) of
    {ok, NumRows} ->
      case NumRows > 0 of
        true -> sumo_event:dispatch(DocName, deleted_all);
        _    -> ok
      end,
      NumRows;
    Error ->
      throw(Error)
  end.

%% @doc Deletes the doc identified by Id.
-spec delete(schema_name(), user_doc()) -> boolean().
delete(DocName, Id) ->
  IdField = sumo_internal:id_field_name(DocName),
  sumo_event:dispatch(DocName, pre_deleted, [Id]),
  case delete_by(DocName, [{IdField, Id}]) of
    1 -> sumo_event:dispatch(DocName, deleted, [Id]), true;
    0 -> false
  end.

%% @doc Deletes the doc identified by Conditions.
-spec delete_by(schema_name(), conditions()) -> non_neg_integer().
delete_by(DocName, Conditions) ->
  Store = sumo_config:get_store(DocName),
  sumo_event:dispatch(DocName, pre_deleted_total, [Conditions]),
  case sumo_store:delete_by(Store, DocName, Conditions) of
    {ok, 0} ->
      0;
    {ok, NumRows} ->
      sumo_event:dispatch(DocName, deleted_total, [NumRows, Conditions]),
      NumRows;
    Error ->
      throw(Error)
  end.

%% @doc Counts the total number of docs in the given schema name `DocName'.
-spec count(schema_name()) -> non_neg_integer().
count(DocName) ->
  case sumo_store:count(sumo_config:get_store(DocName), DocName) of
    {ok, Total} -> Total;
    Error       -> throw(Error)
  end.

%% @doc Calls the given custom function of a store.
-spec call(schema_name(), atom()) -> term().
call(DocName, Function) ->
  call(DocName, Function, []).

%% @doc Calls the given custom function of a store with the given args.
-spec call(schema_name(), atom(), [term()]) -> term().
call(DocName, Function, Args) ->
  Store = sumo_config:get_store(DocName),
  case sumo_store:call(Store, DocName, Function, Args) of
    {ok, {docs, Docs}} -> docs_wakeup(Docs);
    {ok, {raw, Value}} -> Value
  end.

%% @doc Creates the schema for all known (configured) docs.
-spec create_schema() -> ok.
create_schema() ->
  lists:foreach(fun({DocName, _, _}) ->
    create_schema(DocName)
  end, get_docs()).

%% @doc Creates the schema for the docs of type DocName.
-spec create_schema(schema_name()) -> ok.
create_schema(DocName) ->
  create_schema(DocName, sumo_config:get_store(DocName)).

%% @doc
%% Creates the schema for the docs of type `DocName' using the given `Store'.
%% @end
-spec create_schema(schema_name(), atom()) -> ok.
create_schema(DocName, Store) ->
  sumo_event:dispatch(DocName, pre_schema_created),
  case sumo_store:create_schema(Store, sumo_internal:get_schema(DocName)) of
    ok    -> sumo_event:dispatch(DocName, schema_created), ok;
    Error -> throw(Error)
  end.

%% @doc Returns a new schema.
-spec new_schema(schema_name(), [field()]) -> schema().
new_schema(Name, Fields) ->
  sumo_internal:new_schema(Name, Fields).

%% @doc Returns a new field of the given type and attributes.
-spec new_field(field_name(), field_type(), field_attrs()) -> field().
new_field(Name, Type, Attributes) ->
  sumo_internal:new_field(Name, Type, Attributes).

%% @doc Returns a new field of the given type without attributes.
-spec new_field(field_name(), field_type()) -> field().
new_field(Name, Type) ->
  new_field(Name, Type, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
docs_wakeup(Docs) ->
  lists:map(fun(Doc) -> sumo_internal:wakeup(Doc) end, Docs).

%% @private
normalize_sort_fields(FieldName) when is_atom(FieldName) ->
  [{FieldName, asc}];
normalize_sort_fields({Name, Order}) ->
  [{Name, Order}];
normalize_sort_fields(SortFields) when is_list(SortFields) ->
  lists:flatmap(fun normalize_sort_fields/1, SortFields).

%% @private
get_docs() ->
  application:get_env(sumo_db, docs, []).
