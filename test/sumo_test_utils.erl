-module(sumo_test_utils).
-author('elbrujohalcon@inaka.net').

-export(
  [ start_apps/0
  , all_people/0
  , people_with_sort/0
  , people_with_conditional_logic/0
  , people_with_like/0
  ]).

-spec start_apps() -> ok.
start_apps() ->
  application:ensure_all_started(emysql),
  application:ensure_all_started(epgsql),
  application:ensure_all_started(emongo),
  application:ensure_all_started(tirerl),
  mnesia:create_schema([node()]),
  application:ensure_all_started(mnesia),
  application:ensure_all_started(sumo_db).

-spec all_people() -> [atom()].
all_people() ->
  [Doc || {Doc, _Store} <- application:get_env(sumo_db, docs, [])].

-spec people_with_conditional_logic() -> [atom()].
people_with_conditional_logic() ->
  all_people() -- [sumo_test_people_elasticsearch, sumo_test_people_riak].

-spec people_with_sort() -> [atom()].
people_with_sort() ->
  all_people() --
    [ sumo_test_people_elasticsearch
    , sumo_test_people_riak
    , sumo_test_people_mnesia
    ].

-spec people_with_like() -> [atom()].
people_with_like() ->
  all_people() --
    [ sumo_test_people_elasticsearch
    , sumo_test_people_mnesia
    ].
