-type sumo_schema_name():: atom().

-type sumo_field_attr():: id|unique|index|not_null|auto_increment|{length, integer()}.
-type sumo_field_attrs():: [sumo_field_attr()].

-type sumo_field_type():: integer|string|binary|text.
-type sumo_field_name():: atom().
-type sumo_field_value():: term().
-type sumo_field():: {sumo_field_name(), sumo_field_value()}.

-record(sumo_doc, {
  name :: atom(),
  fields=[] :: proplists:proplist()
}).

-record(sumo_field, {
  name :: atom(),
  type :: atom(),
  attrs=[] :: sumo_field_attrs()
}).

-record(sumo_schema, {
  name :: atom(),
  fields=[] :: [#sumo_field{}]
}).
