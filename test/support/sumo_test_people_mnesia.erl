-module(sumo_test_people_mnesia).

-behaviour(sumo_doc).

%% @todo remove this once mixer migrates specs better
-dialyzer([no_behaviours]).

-compile({parse_transform, fancyflow_trans}).

-include_lib("mixer/include/mixer.hrl").
-mixin([
  {sumo_test_people, [
    sumo_wakeup/1,
    sumo_sleep/1,
    new/2,
    new/3,
    new/4,
    from_map/1,
    id/1,
    name/1,
    last_name/1,
    age/1,
    address/1,
    birthdate/1,
    created_at/1,
    height/1,
    description/1,
    profile_image/1,
    is_blocked/1,
    status/1,
    weird_field1/1,
    weird_field2/1,
    weird_field3/1
  ]}
]).

-export([sumo_schema/0]).

%%%=============================================================================
%%% sumo_doc callbacks
%%%=============================================================================

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields = [
    sumo:new_field(id,            integer, [id]),
    sumo:new_field(name,          string),
    sumo:new_field(last_name,     string),
    sumo:new_field(age,           integer),
    sumo:new_field(address,       string),
    sumo:new_field(birthdate,     date),
    sumo:new_field(created_at,    datetime),
    sumo:new_field(height,        float),
    sumo:new_field(description,   string),
    sumo:new_field(profile_image, binary),
    sumo:new_field(is_blocked,    boolean),
    sumo:new_field(status,        string),
    sumo:new_field(weird_field1,  custom, [{type, term}]),
    sumo:new_field(weird_field2,  custom, [{type, list}]),
    sumo:new_field(weird_field3,  custom, [{type, map}])
  ],
  sumo:new_schema(people, Fields).
