-module(sumo_find_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         find_by_sort/1,
         find_all_sort/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite,
         log
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(emysql),
  application:ensure_all_started(emongo),
  application:ensure_all_started(sumo_db),

  sumo:create_schema(sumo_test_people),
  sumo:delete_all(sumo_test_people),

  sumo:persist(sumo_test_people, sumo_test_people:new("A", "E", 6)),
  sumo:persist(sumo_test_people, sumo_test_people:new("B", "D", 3)),
  sumo:persist(sumo_test_people, sumo_test_people:new("C", "C", 5)),
  sumo:persist(sumo_test_people, sumo_test_people:new("D", "B", 4)),
  sumo:persist(sumo_test_people, sumo_test_people:new("E", "A", 2)),
  sumo:persist(sumo_test_people, sumo_test_people:new("F", "E", 1)),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_by_sort(_Config) ->
  [First, Second | _] =
    sumo:find_by(sumo_test_people, [{age, '>', 2}], age, 0, 0),
  <<"B">> = sumo_test_people:name(First),
  <<"D">> = sumo_test_people:name(Second),

  [First1, Second1 | _] =
    sumo:find_by(sumo_test_people,
                 [{age, '>', 2}, {age, '=<', 5}],
                 {age, desc},
                 0,
                 0),
  <<"C">> = sumo_test_people:name(First1),
  <<"D">> = sumo_test_people:name(Second1),

  [_, _, _] =
    sumo:find_by(sumo_test_people, [{age, '>', 2}, {age, '=<', 5}], 0, 0).

find_all_sort(_Config) ->
  [First, Second | _] = sumo:find_all(sumo_test_people, age, 0, 0),
  <<"F">> = sumo_test_people:name(First),
  <<"E">> = sumo_test_people:name(Second),

  [First1, Second1 | _] = sumo:find_all(sumo_test_people,
                                        [{last_name, desc}, {age, asc}],
                                        0,
                                        0),
  <<"F">> = sumo_test_people:name(First1),
  <<"A">> = sumo_test_people:name(Second1),

  [First2, Second2 | _] = sumo:find_all(sumo_test_people, last_name, 0, 0),
  <<"E">> = sumo_test_people:name(First2),
  <<"D">> = sumo_test_people:name(Second2).
