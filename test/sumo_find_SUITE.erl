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
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),

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

  lists:foreach(Fun, sumo_test_utils:people_with_sort()),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_by_sort(_Config) ->
  lists:foreach(
    fun do_find_by_sort/1,
    sumo_test_utils:people_with_sort()).

do_find_by_sort(Module) ->
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

find_all_sort(_Config) ->
  lists:foreach(
    fun do_find_all_sort/1,
    sumo_test_utils:people_with_sort()).

do_find_all_sort(Module) ->
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
