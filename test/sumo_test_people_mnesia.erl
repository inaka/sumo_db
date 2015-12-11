-module(sumo_test_people_mnesia).

-behavior(sumo_doc).

-include_lib("mixer/include/mixer.hrl").
-mixin([{sumo_test_people,
         [sumo_wakeup/1,
          sumo_sleep/1,
          new/2,
          new/3,
          new/4,
          name/1,
          id/1,
          age/1,
          birthdate/1,
          created_at/1
         ]
        }
       ]).

-export([sumo_schema/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,         integer, [id, auto_increment]),
     sumo:new_field(name,       string, [{length, 255}, not_null, unique]),
     sumo:new_field(last_name,  string, [{length, 255}, not_null]),
     sumo:new_field(age,        integer),
     sumo:new_field(address,    string, [{length, 255}]),
     sumo:new_field(birthdate,  date),
     sumo:new_field(created_at, datetime)
    ],
  sumo:new_schema(?MODULE, Fields).
