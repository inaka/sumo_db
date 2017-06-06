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

%%% API for doc/schema manipulation.
-export([new_schema/2, new_field/3]).

%%% API for opaqueness
-export([
  schema_name/1,
  schema_fields/1,
  doc_name/1,
  doc_module/1,
  doc_fields/1,
  wakeup/1,
  new_doc/1,
  new_doc/2,
  from_user_doc/2
]).

%%% API for schema fields manipulation.
-export([
  get_schema/1,
  field_name/1,
  field_type/1,
  field_attrs/1,
  field_is/2,
  get_field/2,
  set_field/3,
  id_field_name/1,
  id_field_type/1
]).

%%% API for conditional logic.
-export([check_operator/1]).

-export([report_overrun/1]).

%%%===================================================================
%%% Types definitions
%%%===================================================================

-opaque schema() :: #{
  name   => sumo:schema_name(),
  fields => [field()]
}.

-opaque doc() :: #{
  name   => sumo:schema_name(),
  module => module(),
  fields => sumo:model()
}.

-opaque field()  :: #{
  name  => atom(),
  type  => atom(),
  attrs => sumo:field_attrs()
}.

-export_type([schema/0, doc/0, field/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Returns a new schema.
-spec new_schema(sumo:schema_name(), [field()]) -> schema().
new_schema(Name, Fields) ->
  S = #{name => Name, fields => Fields},
  _ = get_id_field(S),
  S.

%% @doc Returns a new field of the given type and attributes.
-spec new_field(sumo:field_name(), sumo:field_type(), sumo:field_attrs()) -> field().
new_field(Name, Type, Attributes) ->
  #{name => Name, type => Type, attrs => Attributes}.

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

%% @doc Returns the doc name
-spec doc_module(doc()) -> module().
doc_module(Doc) ->
  maps:get(module, Doc, undefined).

-spec doc_fields(doc()) -> sumo:model().
doc_fields(Doc) ->
  maps:get(fields, Doc, []).

%% @doc Wakes up the document
-spec wakeup(doc()) -> sumo:user_doc().
wakeup(Doc) ->
  Module = sumo_config:get_prop_value(doc_name(Doc), module),
  Fields = maps:get(fields, Doc, []),
  Module:sumo_wakeup(Fields).

%% @doc Returns a new doc.
-spec new_doc(sumo:schema_name()) -> doc().
new_doc(Name) ->
  new_doc(Name, #{}).

%% @doc Returns a new doc.
-spec new_doc(sumo:schema_name(), sumo:model()) -> doc().
new_doc(Name, Fields) ->
  Module = sumo_config:get_prop_value(Name, module),
  #{name => Name, module => Module, fields => Fields}.

%% @doc Returns a new doc from the given user doc.
-spec from_user_doc(sumo:schema_name(), sumo:user_doc()) -> doc().
from_user_doc(Name, UserDoc) ->
  Module = sumo_config:get_prop_value(Name, module),
  Fields = Module:sumo_sleep(UserDoc),
  #{name => Name, module => Module, fields => Fields}.

%% @doc Returns the schema for a given DocName.
-spec get_schema(sumo:schema_name()) -> schema().
get_schema(DocName) ->
  Module = sumo_config:get_prop_value(DocName, module),
  Module:sumo_schema().

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

%% @doc Returns the value of a field from a sumo_doc.
-spec get_field(sumo:field_name(), doc()) -> sumo:field_value().
get_field(Name, Doc) ->
  maps:get(Name, doc_fields(Doc), undefined).

%% @doc Sets a value in an sumo_doc.
-spec set_field(FieldName, FieldValue, DocOrModel) -> UpdatedDocOrModel when
  FieldName         :: sumo:field_name(),
  FieldValue        :: sumo:field_value(),
  DocOrModel        :: doc() | sumo:model(),
  UpdatedDocOrModel :: doc() | sumo:model().
set_field(FieldName, FieldValue, Doc = #{fields := Fields}) ->
  maps:put(fields, maps:put(FieldName, FieldValue, Fields), Doc);
set_field(FieldName, FieldValue, Fields) ->
  maps:put(FieldName, FieldValue, Fields).

%% @doc Returns name of field marked as ID for the given schema or doc name.
-spec id_field_name(sumo:schema_name()) -> sumo:field_name().
id_field_name(DocName) ->
  field_name(get_id_field(get_schema(DocName))).

%% @doc Returns type of field marked as ID for the given schema or doc name.
-spec id_field_type(sumo:schema_name()) -> sumo:field_type().
id_field_type(DocName) ->
  field_type(get_id_field(get_schema(DocName))).

%% @doc Checks the operator is known, exit otherwise.
-spec check_operator(sumo:operator()) -> ok.
check_operator('<') -> ok;
check_operator('=<') -> ok;
check_operator('>') -> ok;
check_operator('>=') -> ok;
check_operator('==') -> ok;
check_operator('/=') -> ok;
check_operator('like') -> ok;
check_operator(Op) -> exit({unknown_operator, Op}).

-spec report_overrun(term()) -> ok.
report_overrun(Report) ->
  error_logger:error_msg("~p", [Report]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @doc Returns field marked as ID for the given schema or doc name.
%% @private
get_id_field(_Schema = #{fields := Fields}) ->
  hd(lists:filter(fun(_Field = #{attrs := Attributes}) ->
    lists:member(id, Attributes)
  end, Fields)).
