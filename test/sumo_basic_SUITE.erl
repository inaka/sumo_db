-module(sumo_basic_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2
        ]).

-export([
         find_all/1,
         find_by/1,
         delete_all/1,
         delete/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         init_per_testcase,
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
  application:ensure_all_started(tirerl),
  application:ensure_all_started(sumo_db),

  Config.

init_per_testcase(_, Config) ->
  run_all_stores(fun init_store/1),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_all(_Config) ->
  run_all_stores(fun find_all_module/1).

find_by(_Config) ->
  run_all_stores(fun find_by_module/1).

delete_all(_Config) ->
  run_all_stores(fun delete_all_module/1).

delete(_Config) ->
  run_all_stores(fun delete_module/1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_store(Module) ->
  sumo:create_schema(Module),
  sumo:delete_all(Module),

  sumo:persist(Module, Module:new("A", "E", 6)),
  sumo:persist(Module, Module:new("B", "D", 3)),
  sumo:persist(Module, Module:new("C", "C", 5)),
  sumo:persist(Module, Module:new("D", "B", 4)),
  sumo:persist(Module, Module:new("E", "A", 2)),
  sumo:persist(Module, Module:new("F", "E", 1)).

find_by_module(Module) ->
  [First, Second | _] = sumo:find_by(Module, [{age, '>', 2}], age, 0, 0),

  "B" = to_str(Module:name(First)),
  "D" = to_str(Module:name(Second)).

find_all_module(Module) ->
  All = sumo:find_all(Module, age, 0, 0),
  6 = length(All).

delete_all_module(Module) ->
  sumo:delete_all(Module),
  [] = sumo:find_all(Module, age, 0, 0).

delete_module(Module) ->
  [First | _ ] = All = sumo:find_all(Module, age, 0, 0),
  Id = Module:id(First),
  sumo:delete(Module, Id),
  NewAll = sumo:find_all(Module, age, 0, 0),

  1 = length(All) - length(NewAll).

%%% Helper

-spec run_all_stores(fun()) -> ok.
run_all_stores(Fun) ->
  Modules = [sumo_test_people,
             sumo_test_people_mongo,
             sumo_test_people_elasticsearch],
  lists:foreach(Fun, Modules).

-spec to_str(any()) -> string().
to_str(X) when is_list(X) ->
  X;
to_str(X) when is_binary(X) ->
  binary_to_list(X).
