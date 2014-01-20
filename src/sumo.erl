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

%%% Include standard types.
-include_lib("include/sumo_doc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API for doc/schema manipulation.
-export([new_doc/1, new_doc/2, new_schema/2, new_field/3, new_field/2]).

%%% API for schema fields manipulation.
-export([get_field/2, set_field/3, get_id_field/1, get_schema/1, field_is/2]).
-export([field_name/1, field_type/1, field_attrs/1]).

%%% API for schema creation.
-export([create_schema/0, create_schema/1, create_schema/2]).

%%% API for standard CRUD functions.
-export([persist/2, delete/2, delete_by/2, delete_all/1]).
-export([find/2, find_all/1, find_all/4, find_by/2, find_by/4, find_one/2]).

%%% API for repo handling.
-export([get_repo/1]).
-export([call/2, call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns all the configured docs.
-spec get_docs() -> [{atom(), atom()}].
get_docs() ->
  {ok, Docs} = application:get_env(sumo_db, docs),
  Docs.

%% @doc Returns the process name that handles persistence for the given
%% Doc or DocName.
-spec get_repo(sumo_schema_name() | #sumo_doc{}) -> atom().
get_repo(DocName) when is_atom(DocName) ->
  proplists:get_value(DocName, get_docs());

get_repo(#sumo_doc{name=Name}) ->
  get_repo(Name).

%% @doc Creates the schema for all known (configured) docs.
-spec create_schema() -> ok.
create_schema() ->
  lists:foreach(
    fun({DocName, Repo}) ->
      case sumo:create_schema(DocName, Repo) of
        ok -> ok;
        Error -> throw(Error)
      end
    end,
    get_docs()
  ),
  ok.

%% @doc Returns the schema for a given DocName.
-spec get_schema(sumo_schema_name()) -> #sumo_schema{}.
get_schema(DocName) ->
  DocName:sumo_schema().

%% @doc Returns 1 doc that matches the given Conditions.
-spec find_one(
  sumo_schema_name(), proplists:proplist()
) -> proplists:proplist() | notfound.
find_one(DocName, Conditions) ->
  case find_by(DocName, Conditions, 1, 0) of
    [] -> notfound;
    List -> hd(List)
  end.

%% @doc Returns the doc identified by Id.
-spec find(sumo_schema_name(), term()) -> proplists:proplist() | notfound.
find(DocName, Id) ->
  IdField = get_id_field(get_schema(DocName)),
  find_one(DocName, [{IdField#sumo_field.name, Id}]).

%% @doc Returns all docs from the given repo.
find_all(DocName) ->
  case sumo_repo:find_all(get_repo(DocName), DocName) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> DocName:sumo_wakeup(Doc#sumo_doc.fields) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns Limit docs from the given repo, starting at offset.
find_all(DocName, OrderField, Limit, Offset) ->
  case sumo_repo:find_all(get_repo(DocName), DocName, OrderField, Limit, Offset) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> DocName:sumo_wakeup(Doc#sumo_doc.fields) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
-spec find_by(
  sumo_schema_name(), proplists:proplist(), pos_integer(), pos_integer()
) -> [proplists:proplist()].
find_by(DocName, Conditions, Limit, Offset) ->
  case sumo_repo:find_by(get_repo(DocName), DocName, Conditions, Limit, Offset) of
    {ok, Docs} ->
      lists:reverse(lists:map(
        fun(Doc) -> DocName:sumo_wakeup(Doc#sumo_doc.fields) end, Docs
      ));
    Error -> throw(Error)
  end.

%% @doc Returns *all* docs that match Conditions.
-spec find_by(
  sumo_schema_name(), proplists:proplist()
) -> [proplists:proplist()].
find_by(DocName, Conditions) ->
  case sumo_repo:find_by(get_repo(DocName), DocName, Conditions) of
    {ok, Docs} -> docs_wakeup(DocName, Docs);
    Error -> throw(Error)
  end.

%% @doc Creates or updates the given Doc.
-spec persist(sumo_schema_name(), proplist:proplists()) -> ok.
persist(DocName, State) ->
  IdField = field_name(get_id_field(DocName)),
  PropList = DocName:sumo_sleep(State),
  EventName = case proplists:get_value(IdField, PropList) of
    undefined -> created;
    _ -> updated
  end,
  case sumo_repo:persist(get_repo(DocName), sumo:new_doc(DocName, PropList)) of
    {ok, NewDoc} ->
      Ret = DocName:sumo_wakeup(NewDoc#sumo_doc.fields),
      sumo_event:dispatch(DocName, EventName, [Ret]),
      Ret;
    Error -> throw(Error)
  end.

%% @doc Deletes all docs of type DocName.
-spec delete_all(sumo_schema_name()) -> ok.
delete_all(DocName) ->
  case sumo_repo:delete_all(get_repo(DocName), DocName) of
    {ok, NumRows} ->
      if
        NumRows > 0 -> sumo_event:dispatch(DocName, deleted_all);
        true -> ok
      end,
      NumRows;
    Error -> throw(Error)
  end.

%% @doc Deletes the doc identified by Id.
-spec delete(sumo_schema_name(), term()) -> ok.
delete(DocName, Id) ->
  IdField = sumo:field_name(sumo:get_id_field(DocName)),
  case delete_by(DocName, [{IdField, Id}]) of
    1 -> sumo_event:dispatch(DocName, deleted, [Id]), true;
    0 -> false
  end.

%% @doc Deletes the doc identified by Conditions.
-spec delete_by(sumo_schema_name(), term()) -> ok.
delete_by(DocName, Conditions) ->
  case sumo_repo:delete_by(get_repo(DocName), DocName, Conditions) of
    {ok, 0} -> 0;
    {ok, NumRows} -> sumo_event:dispatch(DocName, deleted_total, [NumRows]), NumRows;
    Error -> throw(Error)
  end.

%% @doc Creates the schema for the docs of type DocName.
-spec create_schema(sumo_schema_name()) -> ok.
create_schema(DocName) ->
  create_schema(DocName, get_repo(DocName)).

%% @doc Creates the schema for the docs of type DocName using the given
%% repository.
-spec create_schema(sumo_schema_name(), atom()) -> ok.
create_schema(DocName, Repo) ->
  case sumo_repo:create_schema(Repo, get_schema(DocName)) of
    ok ->
      sumo_event:dispatch(DocName, schema_created),
      ok;
    Error -> throw(Error)
  end.

%% @doc Calls the given custom function of a repo.
-spec call(sumo_schema_name(), atom()) -> term().
call(DocName, Function) ->
  call(DocName, Function, []).

%% @doc Calls the given custom function of a repo with the given args.
-spec call(sumo_schema_name(), atom(), [term()]) -> term().
call(DocName, Function, Args) ->
  case sumo_repo:call(get_repo(DocName), DocName, Function, Args) of
    {ok, {docs, Docs}} -> docs_wakeup(DocName, Docs);
    {ok, {raw, Value}} -> Value
  end.

%% @doc Transforms the given #sumo_docs{} into proplists.
-spec docs_wakeup(
  sumo_schema_name(), [#sumo_doc{}]
) -> [proplists:proplist()].
docs_wakeup(DocName, Docs) ->
  lists:reverse(lists:map(
    fun(Doc) ->
      DocName:sumo_wakeup(Doc#sumo_doc.fields)
    end,
    Docs
  )).

%% @doc Returns the value of a field from a sumo_doc.
-spec get_field(sumo_field_name(), #sumo_doc{}) -> term().
get_field(Name, #sumo_doc{fields=Fields}) ->
  proplists:get_value(Name, Fields).

%% @doc Sets a value in an sumo_doc.
-spec set_field(sumo_field_name(), term(), #sumo_doc{}) -> #sumo_doc{}.
set_field(FieldName, Value, #sumo_doc{fields=Fields, name=Name}) ->
  new_doc(Name, lists:keystore(FieldName, 1, Fields, {FieldName, Value})).

%% @doc Returns field marked as ID for the given schema or doc name.
-spec get_id_field(#sumo_schema{}) -> #sumo_field{}.
get_id_field(#sumo_schema{fields=Fields}) ->
  hd(lists:filter(
    fun(#sumo_field{attrs=Attributes}) ->
      length(lists:filter(fun(T) -> T =:= id end, Attributes)) > 0
    end,
    Fields
  ));

get_id_field(DocName) when is_atom(DocName) ->
  get_id_field(get_schema(DocName)).

%% @doc Returns the name of the given field.
-spec field_name(#sumo_field{}) -> sumo_field_name().
field_name(#sumo_field{name=Name}) ->
  Name.

%% @doc Returns the type of the given field.
-spec field_type(#sumo_field{}) -> sumo_field_type().
field_type(#sumo_field{type=Type}) ->
  Type.

%% @doc Returns all attributes of the given field.
-spec field_attrs(#sumo_field{}) -> sumo_field_attrs().
field_attrs(#sumo_field{attrs=Attributes}) ->
  Attributes.

%% @doc True if the field has a given attribute.
-spec field_is(atom(), #sumo_field{}) -> boolean().
field_is(What, #sumo_field{attrs=Attributes}) ->
  proplists:is_defined(What, Attributes).

%% @doc Returns a new doc without any fields.
-spec new_doc(sumo_schema_name()) -> #sumo_doc{}.
new_doc(Name) ->
  new_doc(Name, []).

%% @doc Returns a new doc.
-spec new_doc(sumo_schema_name(), [sumo_field()]) -> #sumo_doc{}.
new_doc(Name, Fields) ->
  #sumo_doc{name=Name, fields=Fields}.

%% @doc Returns a new schema.
-spec new_schema(sumo_schema_name(), [#sumo_field{}]) -> #sumo_schema{}.
new_schema(Name, Fields) ->
  #sumo_schema{name=Name, fields=Fields}.

%% @doc Returns a new field of the given type and attributes.
-spec new_field(
  sumo_field_name(), sumo_field_type(), [sumo_field_attrs()]
) -> #sumo_field{}.
new_field(Name, Type, Attributes) ->
  #sumo_field{name=Name, type=Type, attrs=Attributes}.

%% @doc Returns a new field of the given type without attributes.
-spec new_field(sumo_field_name(), sumo_field_type()) -> #sumo_field{}.
new_field(Name, Type) ->
  new_field(Name, Type, []).
