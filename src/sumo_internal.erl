%%% @doc Main **internal** module for sumo. Use this one from your own applications.
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
-module(sumo_internal).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-author("Brujo Benavides <elbrujohalcon@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(sumo_doc, {
  name :: atom(),
  fields=[] :: sumo:doc()
}).

-record(sumo_field, {
  name :: atom(),
  type :: atom(),
  attrs=[] :: sumo:field_attrs()
}).

-record(sumo_schema, {
  name :: atom(),
  fields=[] :: [field()]
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API for doc/schema manipulation.
-export([new_schema/2, new_field/3]).

%%% API for schema fields manipulation.
-export([get_field/2, set_field/3, id_field_name/1, get_schema/1, field_is/2]).
-export([field_name/1, field_type/1, field_attrs/1]).

%%% API for repo handling.
-export([get_repo/1]).

%%% API for opaqueness
-export([wakeup/2, doc_name/1, doc_fields/1, schema_name/1, schema_fields/1,
         new_doc/2]).

-opaque schema()  :: #sumo_schema{}.
-opaque doc()     :: #sumo_doc{}.
-opaque field()   :: #sumo_field{}.

-export_type([schema/0, doc/0, field/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the name of the schema
-spec schema_name(schema()) -> sumo:schema_name().
schema_name(Schema) ->
  Schema#sumo_schema.name.

%% @doc Returns the fields of the schema
-spec schema_fields(schema()) -> [field()].
schema_fields(Schema) ->
  Schema#sumo_schema.fields.

%% @doc Returns the doc name
-spec doc_name(doc()) -> atom().
doc_name(Doc) ->
  Doc#sumo_doc.name.

-spec doc_fields(doc()) -> sumo:doc().
doc_fields(Doc) ->
  Doc#sumo_doc.fields.

%% @doc Wakes up the document
-spec wakeup(module(), doc()) -> term().
wakeup(DocName, Doc) ->
  DocName:sumo_wakeup(Doc#sumo_doc.fields).

%% @doc Returns all the configured docs.
-spec get_docs() -> [{atom(), atom()}].
get_docs() ->
  {ok, Docs} = application:get_env(sumo_db, docs),
  Docs.

%% @doc Returns the process name that handles persistence for the given
%% Doc or DocName.
-spec get_repo(sumo:schema_name() | doc()) -> atom().
get_repo(DocName) when is_atom(DocName) ->
  proplists:get_value(DocName, get_docs());

get_repo(#sumo_doc{name=Name}) ->
  get_repo(Name).

%% @doc Returns the schema for a given DocName.
-spec get_schema(sumo:schema_name()) -> schema().
get_schema(DocName) ->
  DocName:sumo_schema().

%% @doc Returns the value of a field from a sumo_doc.
-spec get_field(sumo:field_name(), doc()) -> sumo:field_value().
get_field(Name, #sumo_doc{fields=Fields}) ->
  proplists:get_value(Name, Fields).

%% @doc Sets a value in an sumo_doc.
-spec set_field(sumo:field_name(), term(), doc()) -> doc().
set_field(FieldName, Value, #sumo_doc{fields=Fields, name=Name}) ->
  new_doc(Name, lists:keystore(FieldName, 1, Fields, {FieldName, Value})).

%% @doc Returns name of field marked as ID for the given schema or doc name.
-spec id_field_name(sumo:schema_name()) -> sumo:field_name().
id_field_name(DocName) ->
  field_name(get_id_field(get_schema(DocName))).

%% @doc Returns field marked as ID for the given schema or doc name.
get_id_field(#sumo_schema{fields=Fields}) ->
  hd(lists:filter(
    fun(#sumo_field{attrs=Attributes}) ->
      length(lists:filter(fun(T) -> T =:= id end, Attributes)) > 0
    end,
    Fields
  )).

%% @doc Returns the name of the given field.
-spec field_name(field()) -> sumo:field_name().
field_name(#sumo_field{name=Name}) ->
  Name.

%% @doc Returns the type of the given field.
-spec field_type(field()) -> sumo:field_type().
field_type(#sumo_field{type=Type}) ->
  Type.

%% @doc Returns all attributes of the given field.
-spec field_attrs(field()) -> sumo:field_attrs().
field_attrs(#sumo_field{attrs=Attributes}) ->
  Attributes.

%% @doc True if the field has a given attribute.
-spec field_is(atom(), field()) -> boolean().
field_is(What, #sumo_field{attrs=Attributes}) ->
  proplists:is_defined(What, Attributes).

%% @doc Returns a new doc.
-spec new_doc(sumo:schema_name(), [sumo:field()]) -> doc().
new_doc(Name, Fields) ->
  #sumo_doc{name=Name, fields=Fields}.

%% @doc Returns a new schema.
-spec new_schema(sumo:schema_name(), [field()]) -> schema().
new_schema(Name, Fields) ->
  #sumo_schema{name=Name, fields=Fields}.

%% @doc Returns a new field of the given type and attributes.
-spec new_field(
  sumo:field_name(), sumo:field_type(), sumo:field_attrs()
) -> field().
new_field(Name, Type, Attributes) ->
  #sumo_field{name=Name, type=Type, attrs=Attributes}.
