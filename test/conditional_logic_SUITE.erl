-module(conditional_logic_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  dates/1,
  backward_compatibility/1,
  or_conditional/1,
  and_conditional/1,
  not_null_conditional/1,
  null_conditional/1,
  operators/1,
  deeply_nested/1
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
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

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

dates(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("dates with ~p", [Module]),

  5 = length(sumo:find_all(Module)),

  Now = {Today, _} = calendar:universal_time(),

  0 = length(sumo:find_by(Module, [{birthdate, '>', Today}])),
  5 = length(sumo:find_by(Module, [{birthdate, '==', Today}])),
  5 = length(sumo:find_by(Module, [{birthdate, '=<', Today}])),
  0 = length(sumo:find_by(Module, [{created_at, '>', Now}])),
  5 = length(sumo:find_by(Module, [{created_at, '=<', Now}])).

backward_compatibility(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("backward_compatibility with ~p", [Module]),

  5 = length(sumo:find_all(Module)),
  3 = length(sumo:find_by(Module, [{last_name, "Doe"}])),
  1 = length(sumo:find_by(Module, [{name, "Jane"}, {last_name, "Doe"}])).

or_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("or_conditional with ~p", [Module]),

  3 = length(sumo:find_by(Module,
    {'or', [
      {name, "John"},
      {name, "Joe"},
      {name, "Alan"}
    ]}
  )),

  2 = length(sumo:find_by(Module, [
    {last_name, "Doe"},
    {'or', [
      {name, "Jane"},
      {name, "Jane Jr."}
    ]}
  ])).

and_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("and_conditional with ~p", [Module]),

  0 = length(sumo:find_by(Module,
    {'and', [
      {name, "John"},
      {name, "Joe"},
      {name, "Alan"}
    ]}
  )),

  2 = length(sumo:find_by(Module,
    {'and', [
      {last_name, "Doe"},
      {'or', [
        {name, "Jane"},
        {name, "Jane Jr."}
      ]}
    ]}
  )).

not_null_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("not_null_conditional with ~p", [Module]),

  5 = length(sumo:find_by(Module, {age, 'not_null'})),
  5 = length(sumo:find_by(Module, {address, 'not_null'})).

null_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("null_conditional with ~p", [Module]),

  0 = length(sumo:find_by(Module, {age, 'null'})),
  0 = length(sumo:find_by(Module, {address, 'null'})).

operators(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("operators with ~p", [Module]),

  4 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '<', 100}
    ]}
  )),

  1 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '>', 100}
    ]}
  )),

  0 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '>', 102}
    ]}
  )),

  1 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '>=', 102}
    ]}
  )),

  3 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '<', 30}
    ]}
  )),

  4 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '=<', 30}
    ]}
  )),

  1 = length(sumo:find_by(Module,
    {'or', [
      {age, 'null'},
      {age, '==', 30}
    ]}
  )),

  4 = length(sumo:find_by(Module,
    {'and', [
      {age, 'not_null'},
      {age, '/=', 30}
    ]}
  )),

  case sumo_utils:keyfind(people_with_like, Config, false) of
    true ->
      4 = length(sumo:find_by(Module, {name, 'like', "J%"})),
      2 = length(sumo:find_by(Module, {'and', [{name, 'like', "Ja%"}]})),
      1 = length(sumo:find_by(Module, {name, 'like', "A%"})),
      2 = length(sumo:find_by(Module, {name, 'like', "%n"}));
    false ->
      ok
  end.

deeply_nested(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),
  ct:comment("deeply_nested with ~p", [Module]),

  Conditions = {'or', [
    {'and', [
      {age, '>', 100},
      {address, '==', "something"}
    ]},
    {age, 'null'},
    {last_name, "Turing"}
  ]},
  1 = length(sumo:find_by(Module, Conditions)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

init_store(Module) ->
  sumo:create_schema(Module),
  sumo:delete_all(Module),

  sumo:persist(Module, Module:new("Jane", "Doe")),
  sumo:persist(Module, Module:new("John", "Doe", 30)),
  sumo:persist(Module, Module:new("Jane Jr.", "Doe", 5)),
  sumo:persist(Module, Module:new("Joe", "Armstrong")),
  sumo:persist(Module, Module:new("Alan", "Turing", 102, "Computer St.")).
