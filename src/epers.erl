%%% @doc Main module for epers. Use this one from your own applications.
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
-module(epers).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%% Include standard types.
-include_lib("include/epers_doc.hrl").

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
-export([persist/2, delete/2, delete_all/1]).
-export([find/2, find_by/2, find_by/4, find_one/2]).

%%% API for repo handling.
-export([get_repo/1]).
-export([call/2, call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns all the configured docs.
-spec get_docs() -> [{atom(), atom()}].
get_docs() ->
  {ok, Docs} = application:get_env(epers, docs),
  Docs.

%% @doc Returns the process name that handles persistence for the given
%% Doc or DocName.
-spec get_repo(epers_schema_name() | #epers_doc{}) -> atom().
get_repo(DocName) when is_atom(DocName) ->
  proplists:get_value(DocName, get_docs());

get_repo(#epers_doc{name=Name}) ->
  get_repo(Name).

%% @doc Creates the schema for all known (configured) docs.
-spec create_schema() -> ok.
create_schema() ->
  lists:foreach(
    fun({DocName, Repo}) ->
      epers:create_schema(DocName, Repo)
    end,
    get_docs()
  ),
  ok.

%% @doc Returns the schema for a given DocName.
-spec get_schema(epers_schema_name()) -> #epers_schema{}.
get_schema(DocName) ->
  DocName:epers_schema().

%% @doc Returns 1 doc that matches the given Conditions.
-spec find_one(
  epers_schema_name(), proplists:proplist()
) -> proplists:proplist() | notfound.
find_one(DocName, Conditions) ->
  case find_by(DocName, Conditions, 1, 0) of
    [] -> notfound;
    List -> hd(List)
  end.

%% @doc Returns the doc identified by Id.
-spec find(epers_schema_name(), term()) -> proplists:proplist() | notfound.
find(DocName, Id) ->
  IdField = get_id_field(get_schema(DocName)),
  find_one(DocName, [{IdField#epers_field.name, Id}]).

%% @doc Returns Limit number of docs that match Conditions, starting at
%% offset Offset.
-spec find_by(
  epers_schema_name(), proplists:proplist(), pos_integer(), pos_integer()
) -> [proplists:proplist()].
find_by(DocName, Conditions, Limit, Offset) ->
  lists:reverse(lists:map(
    fun(Doc) ->
      DocName:epers_wakeup(Doc)
    end,
    epers_repo:find_by(get_repo(DocName), DocName, Conditions, Limit, Offset)
  )).

%% @doc Returns *all* docs that match Conditions.
-spec find_by(
  epers_schema_name(), proplists:proplist()
) -> [proplists:proplist()].
find_by(DocName, Conditions) ->
  docs_wakeup(
    DocName, epers_repo:find_by(get_repo(DocName), DocName, Conditions)
  ).

%% @doc Creates or updates the given Doc.
-spec persist(epers_schema_name(), proplist:proplists()) -> ok.
persist(DocName, State) ->
  epers_repo:persist(
    get_repo(DocName), epers:new_doc(DocName, DocName:epers_sleep(State))
  ).

%% @doc Deletes all docs of type DocName.
-spec delete_all(epers_schema_name()) -> ok.
delete_all(DocName) ->
  epers_repo:delete_all(get_repo(DocName), DocName).

%% @doc Deletes the doc identified by Id.
-spec delete(epers_schema_name(), term()) -> ok.
delete(DocName, Id) ->
  epers_repo:delete(get_repo(DocName), DocName, Id).

%% @doc Creates the schema for the docs of type DocName.
-spec create_schema(epers_schema_name()) -> ok.
create_schema(DocName) ->
  create_schema(DocName, get_repo(DocName)).

%% @doc Creates the schema for the docs of type DocName using the given
%% repository.
-spec create_schema(epers_schema_name(), atom()) -> ok.
create_schema(DocName, Repo) ->
  epers_repo:create_schema(Repo, get_schema(DocName)).

%% @doc Calls the given custom function of a repo.
-spec call(epers_schema_name(), atom()) -> term().
call(DocName, Function) ->
  call(DocName, Function, []).

%% @doc Calls the given custom function of a repo with the given args.
-spec call(epers_schema_name(), atom(), [term()]) -> term().
call(DocName, Function, Args) ->
  case epers_repo:call(get_repo(DocName), DocName, Function, Args) of
    {docs, Docs} -> docs_wakeup(DocName, Docs);
    {raw, Value} -> Value
  end.

%% @doc Transforms the given #epers_docs{} into proplists.
-spec docs_wakeup(
  epers_schema_name(), [#epers_doc{}]
) -> [proplists:proplist()].
docs_wakeup(DocName, Docs) ->
  lists:reverse(lists:map(
    fun(Doc) ->
      DocName:epers_wakeup(Doc)
    end,
    Docs
  )).

%% @doc Returns the value of a field from an epers_doc.
-spec get_field(epers_field_name(), #epers_doc{}) -> term().
get_field(Name, #epers_doc{fields=Fields}) ->
  proplists:get_value(Name, Fields).

%% @doc Sets a value in an epers_doc.
-spec set_field(epers_field_name(), term(), #epers_doc{}) -> #epers_doc{}.
set_field(FieldName, Value, #epers_doc{fields=Fields, name=Name}) ->
  new_doc(Name, lists:keystore(FieldName, 1, Fields, {FieldName, Value})).

%% @doc Returns field marked as ID for the given schema or doc name.
-spec get_id_field(#epers_schema{}) -> #epers_field{}.
get_id_field(#epers_schema{fields=Fields}) ->
  hd(lists:filter(
    fun(#epers_field{attrs=Attributes}) ->
      length(lists:filter(fun(T) -> T =:= id end, Attributes)) > 0
    end,
    Fields
  ));

get_id_field(DocName) when is_atom(DocName) ->
  get_id_field(get_schema(DocName)).

%% @doc Returns the name of the given field.
-spec field_name(#epers_field{}) -> epers_field_name().
field_name(#epers_field{name=Name}) ->
  Name.

%% @doc Returns the type of the given field.
-spec field_type(#epers_field{}) -> epers_field_type().
field_type(#epers_field{type=Type}) ->
  Type.

%% @doc Returns all attributes of the given field.
-spec field_attrs(#epers_field{}) -> epers_field_attrs().
field_attrs(#epers_field{attrs=Attributes}) ->
  Attributes.

%% @doc True if the field has a given attribute.
-spec field_is(atom(), #epers_field{}) -> boolean().
field_is(What, #epers_field{attrs=Attributes}) ->
  proplists:is_defined(What, Attributes).

%% @doc Returns a new doc without any fields
-spec new_doc(epers_schema_name()) -> #epers_doc{}.
new_doc(Name) ->
  new_doc(Name, []).

%% @doc Returns a new doc.
-spec new_doc(epers_schema_name(), [epers_field()]) -> #epers_doc{}.
new_doc(Name, Fields) ->
  #epers_doc{name=Name, fields=Fields}.

%% @doc Returns a new schema.
-spec new_schema(epers_schema_name(), [#epers_field{}]) -> #epers_schema{}.
new_schema(Name, Fields) ->
  #epers_schema{name=Name, fields=Fields}.

%% @doc Returns a new field of the given type and attributes.
-spec new_field(
  epers_field_name(), epers_field_type(), [epers_field_attrs()]
) -> #epers_field{}.
new_field(Name, Type, Attributes) ->
  #epers_field{name=Name, type=Type, attrs=Attributes}.

%% @doc Returns a new field of the given type without attributes.
-spec new_field(epers_field_name(), epers_field_type()) -> #epers_field{}.
new_field(Name, Type) ->
  new_field(Name, Type, []).
