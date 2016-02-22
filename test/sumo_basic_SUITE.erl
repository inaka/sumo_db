-module(sumo_basic_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2
]).

%% Test Cases
-export([
  find/1,
  find_all/1,
  find_by/1,
  delete_all/1,
  delete/1,
  check_proper_dates/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  init_per_testcase,
  end_per_suite
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common Test
%%%=============================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),
  [{module, sumo_test_people_mnesia} | Config].

init_per_testcase(_, Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  init_store(Module),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

find(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [First, Second | _] = sumo:find_all(Module),
  First = sumo:find(Module, Module:id(First)),
  Second = sumo:find(Module, Module:id(Second)),
  notfound = sumo:find(Module, 0).

find_all(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  8 = length(sumo:find_all(Module)).

find_by(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  Results = sumo:find_by(Module, [{last_name, <<"D">>}]),
  2 = length(Results),
  SortFun = fun(A, B) -> Module:name(A) < Module:name(B) end,
  [First, Second | _] = lists:sort(SortFun, Results),

  {Today, _} = calendar:universal_time(),

  "B" = Module:name(First),
  "D" = Module:name(Second),
  3 = Module:age(First),
  4 = Module:age(Second),
  "D" = Module:last_name(First),
  "" = Module:address(First),
  Today = Module:birthdate(First),
  0.0 = Module:height(First),
  <<>> = Module:description(First),
  {Today, _} = Module:created_at(First),
  % Check that it returns what we have inserted
  [LastPerson | _NothingElse] = sumo:find_by(Module, [{last_name, "LastName"}]),
  "Name" = Module:name(LastPerson),
  "LastName" = Module:last_name(LastPerson),
  3 = Module:age(LastPerson),
  "" = Module:address(LastPerson),
  {Date, _} = calendar:universal_time(),
  Date = Module:birthdate(LastPerson),
  1.75 = Module:height(LastPerson),
  <<"description">> = Module:description(LastPerson),
  {Today, _} = Module:created_at(LastPerson),

  %% Check find_by ID
  [First1] = sumo:find_by(Module, [{id, Module:id(First)}]),
  First1 = First,
  %% Check pagination
  Results1 = sumo:find_by(Module, [], 3, 1),
  3 = length(Results1),

  %% This test is #177 github issue related
  8 = length(sumo:find_by(Module, [])),
  Robot = sumo:find_by(Module, [{name, <<"Model T-2000">>}]),
  1 = length(Robot).

delete_all(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  sumo:delete_all(Module),
  [] = sumo:find_all(Module).

delete(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  %% delete_by
  2 = sumo:delete_by(Module, [{last_name, <<"D">>}]),
  Results = sumo:find_by(Module, [{last_name, <<"D">>}]),

  0 = length(Results),

  %% delete
  [First | _ ] = All = sumo:find_all(Module),
  Id = Module:id(First),
  sumo:delete(Module, Id),
  NewAll = sumo:find_all(Module),

  1 = length(All) - length(NewAll).

check_proper_dates(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [P0] = sumo:find_by(Module, [{name, <<"A">>}]),
  P1 = sumo:find(Module, Module:id(P0)),
  [P2 | _] = sumo:find_all(Module),

  {Date, _} = calendar:universal_time(),

  Date = Module:birthdate(P0),
  {Date, {_, _, _}} = Module:created_at(P0),
  Date = Module:birthdate(P1),
  {Date, {_, _, _}} = Module:created_at(P1),
  Date = Module:birthdate(P2),
  {Date, {_, _, _}} = Module:created_at(P2),

  Person = sumo:persist(Module, Module:new(<<"X">>, <<"Z">>, 6)),
  Date = Module:birthdate(Person).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

init_store(Module) ->
  sumo:create_schema(Module),
  sumo:delete_all(Module),

  sumo:persist(Module, Module:new(<<"A">>, <<"E">>, 6)),
  sumo:persist(Module, Module:new(<<"B">>, <<"D">>, 3)),
  sumo:persist(Module, Module:new(<<"C">>, <<"C">>, 5)),
  sumo:persist(Module, Module:new(<<"D">>, <<"D">>, 4)),
  sumo:persist(Module, Module:new(<<"E">>, <<"A">>, 2)),
  sumo:persist(Module, Module:new(<<"F">>, <<"E">>, 1)),
  sumo:persist(Module, Module:new(<<"Model T-2000">>, <<"undefined">>, 7)),

  {Date, _} = calendar:universal_time(),
  sumo:persist(Module, Module:new(
    "Name", "LastName", 3, "", Date, 1.75, <<"description">>)).
