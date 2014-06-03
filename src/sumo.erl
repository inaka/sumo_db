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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API for doc/schema manipulation.
-export([new_schema/2, new_field/3, new_field/2]).

%%% API for schema creation.
-export([create_schema/0, create_schema/1, create_schema/2]).

%%% API for standard CRUD functions.
-export([persist/2, delete/2, delete_by/2, delete_all/1]).
-export([find/2, find_all/1, find_all/4, find_by/2, find_by/4, find_one/2]).
-export([call/2, call/3]).

-type schema_name() :: atom().

-type field_attr()  :: id|unique|index|not_null|auto_increment|{length, integer()}.
-type field_attrs() :: [field_attr()].

-type field_type()  :: integer|string|binary|text|float|date|datetime.
-type field_name()  :: atom().
-type field_value() :: term().
-type doc()         :: [{field_name(), field_value()}].
-type conditions()  :: [{field_name(), field_value()}].

-export_type([schema_name/0, field_attr/0, field_attrs/0, field_type/0,
              field_name/0, field_value/0, doc/0, conditions/0]).

-type schema()      :: sumo_internal:schema().
-type field()       :: sumo_internal:field().

-export_type([schema/0, field/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns all the configured docs.
-spec get_docs() -> [{atom(), atom()}].
get_docs() ->
  {ok, Docs} = application:get_env(sumo_db, docs),
  Docs.

%% @doc Creates the schema for all known (configured) docs.
-spec create_schema() -> ok.
create_schema() ->
  lists:foreach(
    fun({DocName, Repo}) ->
      case create_schema(DocName, Repo) of
        ok -> ok;
        Error -> throw(Error)
      end
    end,
    get_docs()
  ),
  ok.

%% @doc Returns 1 doc that matches the given Conditions.
-spec find_one(sumo:schema_name(), conditions()) -> doc() | notfound.
find_one(DocName, Conditions) ->
  case find_by(DocName, Conditions, 1, 0) of
    [] -> notfound;
    List -> hd(List)
  end.

%% @doc Returns the doc identified by Id.
-spec find(sumo:schema_name(), term()) -> doc() | notfound.
find(DocName, Id) ->
  IdFieldName = sumo_internal:id_field_name(DocName),
  find_one(DocName, [{IdFieldName, Id}]).

%% @doc Returns all docs from the given repo.
find_all(DocName) ->
  case sumo_repo:find_all(sumo_internal:get_repo(DocName), DocName) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> sumo_internal:wakeup(DocName, Doc) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns Limit docs from the given repo, starting at offset.
find_all(DocName, OrderField, Limit, Offset) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:find_all(Repo, DocName, OrderField, Limit, Offset) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> sumo_internal:wakeup(DocName, Doc) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
-spec find_by(
  sumo:schema_name(), conditions(), pos_integer(), pos_integer()
) -> [doc()].
find_by(DocName, Conditions, Limit, Offset) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:find_by(Repo, DocName, Conditions, Limit, Offset) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> sumo_internal:wakeup(DocName, Doc) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns *all* docs that match Conditions.
-spec find_by(sumo:schema_name(), conditions()) -> [doc()].
find_by(DocName, Conditions) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:find_by(Repo, DocName, Conditions) of
    {ok, Docs} -> docs_wakeup(DocName, Docs);
    Error -> throw(Error)
  end.

%% @doc Creates or updates the given Doc.
-spec persist(sumo:schema_name(), term()) -> ok.
persist(DocName, State) ->
  IdField = sumo_internal:id_field_name(DocName),
  PropList = DocName:sumo_sleep(State),
  EventName = case proplists:get_value(IdField, PropList) of
    undefined -> created;
    _ -> updated
  end,
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:persist(Repo, sumo_internal:new_doc(DocName, PropList)) of
    {ok, NewDoc} ->
      Ret = sumo_internal:wakeup(DocName, NewDoc),
      sumo_event:dispatch(DocName, EventName, [Ret]),
      Ret;
    Error -> throw(Error)
  end.

%% @doc Deletes all docs of type DocName.
-spec delete_all(sumo:schema_name()) -> ok.
delete_all(DocName) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:delete_all(Repo, DocName) of
    {ok, NumRows} ->
      if
        NumRows > 0 -> sumo_event:dispatch(DocName, deleted_all);
        true -> ok
      end,
      NumRows;
    Error -> throw(Error)
  end.

%% @doc Deletes the doc identified by Id.
-spec delete(sumo:schema_name(), term()) -> ok.
delete(DocName, Id) ->
  IdField = sumo_internal:id_field_name(DocName),
  case delete_by(DocName, [{IdField, Id}]) of
    1 -> sumo_event:dispatch(DocName, deleted, [Id]), true;
    0 -> false
  end.

%% @doc Deletes the doc identified by Conditions.
-spec delete_by(sumo:schema_name(), term()) -> ok.
delete_by(DocName, Conditions) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:delete_by(Repo, DocName, Conditions) of
    {ok, 0} -> 0;
    {ok, NumRows} -> sumo_event:dispatch(DocName, deleted_total, [NumRows]), NumRows;
    Error -> throw(Error)
  end.

%% @doc Creates the schema for the docs of type DocName.
-spec create_schema(sumo:schema_name()) -> ok.
create_schema(DocName) ->
  create_schema(DocName, sumo_internal:get_repo(DocName)).

%% @doc Creates the schema for the docs of type DocName using the given
%% repository.
-spec create_schema(sumo:schema_name(), atom()) -> ok.
create_schema(DocName, Repo) ->
  case sumo_repo:create_schema(Repo, sumo_internal:get_schema(DocName)) of
    ok ->
      sumo_event:dispatch(DocName, schema_created),
      ok;
    Error -> throw(Error)
  end.

%% @doc Calls the given custom function of a repo.
-spec call(sumo:schema_name(), atom()) -> term().
call(DocName, Function) ->
  call(DocName, Function, []).

%% @doc Calls the given custom function of a repo with the given args.
-spec call(sumo:schema_name(), atom(), [term()]) -> term().
call(DocName, Function, Args) ->
  Repo = sumo_internal:get_repo(DocName),
  case sumo_repo:call(Repo, DocName, Function, Args) of
    {ok, {docs, Docs}} -> docs_wakeup(DocName, Docs);
    {ok, {raw, Value}} -> Value
  end.

docs_wakeup(DocName, Docs) ->
  lists:reverse(lists:map(
    fun(Doc) ->
      sumo_internal:wakeup(DocName, Doc)
    end,
    Docs
  )).

%% @doc Returns a new schema.
-spec new_schema(sumo:schema_name(), [field()]) -> schema().
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
