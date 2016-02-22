-module(sumo_find_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).


%% Test Cases
-export([
  find_by_sort/1,
  find_all_sort/1
]).

-define(EXCLUDED_FUNS, [
  all,
  module_info,
  init_per_suite,
  end_per_suite
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common Test
%%%=============================================================================

-spec all() -> [atom()].
all() -> [].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),
  init_store(sumo_test_people_mnesia),
  [{module, sumo_test_people_mnesia} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

find_by_sort(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [First, Second | _] = sumo:find_by(Module, [{age, '>', 2}], age, 0, 0),

  "B" = Module:name(First),
  "D" = Module:name(Second),

  [First1, Second1 | _] = sumo:find_by(Module, [
    {age, '>', 2},
    {age, '=<', 5}
  ], {age, desc}, 0, 0),
  "C" = Module:name(First1),
  "D" = Module:name(Second1),

  3 = length(
    sumo:find_by(Module, [{age, '>', 2}, {age, '=<', 5}], 0, 0)).

find_all_sort( Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [First, Second | _] = sumo:find_all(Module, age, 0, 0),
  "F" = Module:name(First),
  "E" = Module:name(Second),

  [First1, Second1 | _] = sumo:find_all(Module, [
    {last_name, desc},
    {age, asc}
  ], 0, 0),
  "F" = Module:name(First1),
  "A" = Module:name(Second1),

  [First2, Second2 | _] = sumo:find_all(Module, last_name, 0, 0),
  "E" = Module:name(First2),
  "D" = Module:name(Second2).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

init_store(Module) ->
  sumo:create_schema(Module),
  sumo:delete_all(Module),

  sumo:persist(Module, Module:new("A", "E", 6)),
  sumo:persist(Module, Module:new("B", "D", 3)),
  sumo:persist(Module, Module:new("C", "C", 5)),
  sumo:persist(Module, Module:new("D", "B", 4)),
  sumo:persist(Module, Module:new("E", "A", 2)),
  sumo:persist(Module, Module:new("F", "E", 1)).
