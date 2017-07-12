-module(sumo_find_test_helper).

%% Test Cases - Helpers
-export([
  find_by_sort/1,
  find_all_sort/1,
  init_store/1
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
%%%=============================================================================

-spec find_by_sort(config()) -> ok.
find_by_sort(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  [First, Second | _] = sumo:find_by(Name, [{age, '>', 2}], age, 0, 0),

  <<"B">> = Module:name(First),
  <<"D">> = Module:name(Second),

  [First1, Second1 | _] = sumo:find_by(
    Name,
    [{age, '>', 2}, {age, '=<', 5}],
    {age, desc}, 0, 0),
  <<"C">> = Module:name(First1),
  <<"D">> = Module:name(Second1),

  [_, _, _] = sumo:find_by(Name, [{age, '>', 2}, {age, '=<', 5}], 0, 0),
  ok.

-spec find_all_sort(config()) -> ok.
find_all_sort( Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  [First, Second | _] = sumo:find_all(Name, age, 0, 0),
  <<"F">> = Module:name(First),
  <<"E">> = Module:name(Second),

  [First1, Second1 | _] = sumo:find_all(
    Name, [{last_name, desc}, {age, asc}], 0, 0),
  <<"F">> = Module:name(First1),
  <<"A">> = Module:name(Second1),

  [First2, Second2 | _] = sumo:find_all(Name, last_name, 0, 0),
  <<"E">> = Module:name(First2),
  <<"D">> = Module:name(Second2),
  ok.

-spec init_store(module()) -> ok.
init_store(Name) ->
  sumo:create_schema(Name),
  Module = sumo_config:get_prop_value(Name, module),
  sumo:delete_all(Name),

  sumo:persist(Name, Module:new(<<"A">>, <<"E">>, 6)),
  sumo:persist(Name, Module:new(<<"B">>, <<"D">>, 3)),
  sumo:persist(Name, Module:new(<<"C">>, <<"C">>, 5)),
  sumo:persist(Name, Module:new(<<"D">>, <<"B">>, 4)),
  sumo:persist(Name, Module:new(<<"E">>, <<"A">>, 2)),
  sumo:persist(Name, Module:new(<<"F">>, <<"E">>, 1)),
  ok.
