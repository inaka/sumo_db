-module(sumo_find_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         find_sort_mysql/1,
         find_sort_mongo/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite,
         find_by_sort,
         find_all_sort
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

    Fun =
    fun (Module) ->
        sumo:create_schema(Module),
        sumo:delete_all(Module),

        sumo:persist(Module, Module:new("A", "E", 6)),
        sumo:persist(Module, Module:new("B", "D", 3)),
        sumo:persist(Module, Module:new("C", "C", 5)),
        sumo:persist(Module, Module:new("D", "B", 4)),
        sumo:persist(Module, Module:new("E", "A", 2)),
        sumo:persist(Module, Module:new("F", "E", 1))
    end,

  lists:foreach(Fun, [sumo_test_people_mysql, sumo_test_people_mongo]),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_sort_mysql(_Config) ->
  find_by_sort(sumo_test_people_mysql),
  find_all_sort(sumo_test_people_mysql).

find_sort_mongo(_Config) ->
  find_by_sort(sumo_test_people_mongo),
  find_all_sort(sumo_test_people_mongo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_by_sort(Module) ->
  [First, Second | _] = sumo:find_by(Module, [{age, '>', 2}], age, 0, 0),

  "B" = to_str(Module:name(First)),
  "D" = to_str(Module:name(Second)),

  [First1, Second1 | _] =
    sumo:find_by(Module,
                 [{age, '>', 2}, {age, '=<', 5}],
                 {age, desc},
                 0,
                 0),
  "C" = to_str(Module:name(First1)),
  "D" = to_str(Module:name(Second1)),

  [_, _, _] =
    sumo:find_by(Module, [{age, '>', 2}, {age, '=<', 5}], 0, 0).

find_all_sort(Module) ->
  [First, Second | _] = sumo:find_all(Module, age, 0, 0),
  "F" = to_str(Module:name(First)),
  "E" = to_str(Module:name(Second)),

  [First1, Second1 | _] = sumo:find_all(Module,
                                        [{last_name, desc}, {age, asc}],
                                        0,
                                        0),
  "F" = to_str(Module:name(First1)),
  "A" = to_str(Module:name(Second1)),

  [First2, Second2 | _] = sumo:find_all(Module, last_name, 0, 0),
  "E" = to_str(Module:name(First2)),
  "D" = to_str(Module:name(Second2)).

-spec to_str(any()) -> string().
to_str(X) when is_list(X) ->
  X;
to_str(X) when is_binary(X) ->
  binary_to_list(X).
