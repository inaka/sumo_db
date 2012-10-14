-type epers_schema_name():: atom().

-type epers_field_attr():: id|unique|index|not_null|auto_increment|{length, integer()}.
-type epers_field_attrs():: [epers_field_attr()].

-type epers_field_type():: integer|string|binary|text.
-type epers_field_name():: atom().
-type epers_field_value():: term().
-type epers_field():: {epers_field_name(), epers_field_value()}.

-record(epers_doc, {
  name :: atom(),
  fields=[] :: proplists:proplist()
}).

-record(epers_field, {
  name :: atom(),
  type :: atom(),
  attrs=[] :: epers_field_attrs()
}).

-record(epers_schema, {
  name :: atom(),
  fields=[] :: [#epers_field{}]
}).
