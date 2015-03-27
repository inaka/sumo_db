%%% @doc Main **internal** module for sumo.
%%%      Use this one from your own applications.
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
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API for doc/schema manipulation.
-export([new_schema/2, new_field/3]).

%%% API for schema fields manipulation.
-export([get_field/2, set_field/3, id_field_name/1, get_schema/1, field_is/2,
         id_field_type/1]).
-export([field_name/1, field_type/1, field_attrs/1]).

%%% API for store handling.
-export([get_store/1]).

%%% API for opaqueness
-export([wakeup/1, wakeup/2,
         new_doc/1, new_doc/2,
         doc_name/1, doc_fields/1,
         schema_name/1, schema_fields/1]).

%%% API for conditional logic.
-export([check_operator/1]).

-export([report_overrun/1]).

-opaque schema() :: #{name => atom(),
                      fields => [field()]}.

-opaque doc()    :: #{name => atom(),
                      fields => sumo:doc()}.

-opaque field()  :: #{name => atom(),
                      type => atom(),
                      attrs => sumo:field_attrs()}.

-export_type([schema/0, doc/0, field/0]).


%% Conditional Logic

-type operator() :: '<' | '>' | '=' | '<=' | '>=' | '!=' | 'like'.
-type field_name() :: atom().
-type value() :: binary() | string() | number() | 'null' | 'not_null'.

-type expression() ::
    [expression()]
    | {'and', [expression()]}
    | {'or', [expression()]}
    | {'not', expression()}
    | terminal().

-type terminal() ::
    {field_name(), operator(), field_name()}
    | {field_name(), operator(), value()}
    | {field_name(), value()}.

-export_type([expression/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the name of the schema
-spec schema_name(schema()) -> sumo:schema_name().
schema_name(Schema) ->
  maps:get(name, Schema, undefined).

%% @doc Returns the fields of the schema
-spec schema_fields(schema()) -> [field()].
schema_fields(Schema) ->
  maps:get(fields, Schema, []).

%% @doc Returns the doc name
-spec doc_name(doc()) -> atom().
doc_name(Doc) ->
  maps:get(name, Doc, undefined).

-spec doc_fields(doc()) -> sumo:doc().
doc_fields(Doc) ->
  maps:get(fields, Doc, []).

%% @doc Wakes up the document
-spec wakeup(doc()) -> sumo:user_doc().
wakeup(Doc) ->
  wakeup(doc_name(Doc), Doc).

%% @doc Wakes up the document
-spec wakeup(module(), doc()) -> sumo:user_doc().
wakeup(DocName, Doc) ->
  Fields = maps:get(fields, Doc, []),
  DocName:sumo_wakeup(Fields).

%% @doc Returns all the configured docs.
-spec get_docs() -> [{atom(), atom()}].
get_docs() ->
  {ok, Docs} = application:get_env(sumo_db, docs),
  Docs.

%% @doc Returns the process name that handles persistence for the given
%% Doc or DocName.
-spec get_store(sumo:schema_name() | doc()) -> atom().
get_store(DocName) when is_atom(DocName) ->
  proplists:get_value(DocName, get_docs());

get_store(_Doc = #{name := Name}) ->
  get_store(Name).

%% @doc Returns the schema for a given DocName.
-spec get_schema(sumo:schema_name()) -> schema().
get_schema(DocName) ->
  DocName:sumo_schema().

%% @doc Returns the value of a field from a sumo_doc.
-spec get_field(sumo:field_name(), doc()) -> sumo:field_value().
get_field(Name, Doc) ->
  maps:get(Name, doc_fields(Doc), undefined).

%% @doc Sets a value in an sumo_doc.
-spec set_field(sumo:field_name(), sumo:field_value(), doc()) -> doc().
set_field(FieldName, Value, _Doc = #{fields := Fields, name := Name}) ->
  new_doc(Name, maps:put(FieldName, Value, Fields)).

%% @doc Returns name of field marked as ID for the given schema or doc name.
-spec id_field_name(sumo:schema_name()) -> sumo:field_name().
id_field_name(DocName) ->
  field_name(get_id_field(get_schema(DocName))).

%% @doc Returns type of field marked as ID for the given schema or doc name.
-spec id_field_type(sumo:schema_name()) -> sumo:field_type().
id_field_type(DocName) ->
  field_type(get_id_field(get_schema(DocName))).

%% @doc Returns field marked as ID for the given schema or doc name.
get_id_field(_Schema = #{fields := Fields}) ->
  hd(lists:filter(
    fun(_Field = #{attrs := Attributes}) ->
      length(lists:filter(fun(T) -> T =:= id end, Attributes)) > 0
    end,
    Fields
  )).

%% @doc Returns the name of the given field.
-spec field_name(field()) -> sumo:field_name().
field_name(#{name := Name}) ->
  Name.

%% @doc Returns the type of the given field.
-spec field_type(field()) -> sumo:field_type().
field_type(#{type := Type}) ->
  Type.

%% @doc Returns all attributes of the given field.
-spec field_attrs(field()) -> sumo:field_attrs().
field_attrs(_Field = #{attrs := Attributes}) ->
  Attributes.

%% @doc True if the field has a given attribute.
-spec field_is(atom(), field()) -> boolean().
field_is(What, #{attrs := Attributes}) ->
  proplists:is_defined(What, Attributes).

%% @doc Returns a new doc.
-spec new_doc(sumo:schema_name()) -> doc().
new_doc(Name) ->
  new_doc(Name, #{}).

%% @doc Returns a new doc.
-spec new_doc(sumo:schema_name(), sumo:doc()) -> doc().
new_doc(Name, Fields) ->
  #{name => Name,
    fields => Fields}.

%% @doc Returns a new schema.
-spec new_schema(sumo:schema_name(), [field()]) -> schema().
new_schema(Name, Fields) ->
  #{name => Name,
    fields => Fields}.

%% @doc Returns a new field of the given type and attributes.
-spec new_field(
  sumo:field_name(), sumo:field_type(), sumo:field_attrs()
) -> field().
new_field(Name, Type, Attributes) ->
  #{name => Name,
    type => Type,
    attrs => Attributes}.

%% @doc Checks the operator is known, throws otherwise.
-spec check_operator(operator()) -> ok.
check_operator('<') -> ok;
check_operator('=<') -> ok;
check_operator('>') -> ok;
check_operator('>=') -> ok;
check_operator('==') -> ok;
check_operator('/=') -> ok;
check_operator('like') -> ok;
check_operator(Op) -> throw({unknown_operator, Op}).

-spec report_overrun(term()) -> ok.
report_overrun(Report) ->
  lager:error("~p", [Report]).
