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
  application:ensure_all_started(epgsql),
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

  sumo:persist(Module, Module:new(<<"A">>, <<"E">>, 6)),
  sumo:persist(Module, Module:new(<<"B">>, <<"D">>, 3)),
  sumo:persist(Module, Module:new(<<"C">>, <<"C">>, 5)),
  sumo:persist(Module, Module:new(<<"D">>, <<"D">>, 4)),
  sumo:persist(Module, Module:new(<<"E">>, <<"A">>, 2)),
  sumo:persist(Module, Module:new(<<"F">>, <<"E">>, 1)),

  %% This is necessary to get elasticsearch in
  %% particular to index its stuff.
  timer:sleep(1000).

find_all_module(Module) ->
  6 = length(sumo:find_all(Module)).

find_by_module(Module) ->
  Results = sumo:find_by(Module, [{last_name, <<"D">>}]),
  SortFun = fun(A, B) -> Module:name(A) < Module:name(B) end,
  [First, Second | _] = lists:sort(SortFun, Results),

  "B" = to_str(Module:name(First)),
  "D" = to_str(Module:name(Second)).

delete_all_module(Module) ->
  sumo:delete_all(Module),
  [] = sumo:find_all(Module).

delete_module(Module) ->
  [First | _ ] = All = sumo:find_all(Module),
  Id = Module:id(First),
  sumo:delete(Module, Id),
  NewAll = sumo:find_all(Module),

  1 = length(All) - length(NewAll).

%%% Helper

-spec run_all_stores(fun()) -> ok.
run_all_stores(Fun) ->
  Modules = [sumo_test_people_mysql,
             sumo_test_people_mongo,
             sumo_test_people_elasticsearch],
  lists:foreach(Fun, Modules).

-spec to_str(any()) -> string().
to_str(X) when is_list(X) ->
  X;
to_str(X) when is_binary(X) ->
  binary_to_list(X).
