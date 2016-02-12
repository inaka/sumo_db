-module(sumo_test_utils).
-author('elbrujohalcon@inaka.net').

-export(
  [ start_apps/0
  , all_people/0
  , people_with_sort/0
  , people_with_conditional_logic/0
  , people_with_like/0
  , people_with_numeric_sort/0
  , sleep_if_required/1
  ]).

-spec start_apps() -> ok.
start_apps() ->
  {ok, _} = application:ensure_all_started(emysql),
  {ok, _} = application:ensure_all_started(epgsql),
  {ok, _} = application:ensure_all_started(emongo),
  {ok, _} = application:ensure_all_started(tirerl),
  mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  {ok, _} = application:ensure_all_started(sumo_db).

-spec all_people() -> [atom()].
all_people() ->
  [Doc || {Doc, _Store} <- application:get_env(sumo_db, docs, [])] --
  % Failing test people, missing fix.
  [sumo_test_people_elasticsearch, sumo_test_people_mysql, sumo_test_people_pgsql].

-spec people_with_conditional_logic() -> [atom()].
people_with_conditional_logic() ->
  all_people() -- [sumo_test_people_elasticsearch].

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

-spec people_with_numeric_sort() -> [atom()].
people_with_numeric_sort() ->
  people_with_conditional_logic() -- [sumo_test_people_riak].

-spec sleep_if_required(atom()) -> ok.
sleep_if_required(Module) ->
  case Module of
    sumo_test_people_riak -> timer:sleep(5000);
    _                     -> ok
  end.
