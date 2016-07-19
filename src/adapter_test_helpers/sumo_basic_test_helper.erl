-module(sumo_basic_test_helper).

%% Test Cases - Helpers
-export([
  find/1,
  find_all/1,
  find_by/1,
  delete_all/1,
  delete/1,
  check_proper_dates/1,
  init_store/1
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
%%%=============================================================================

-spec find(config()) -> ok.
find(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_internal:get_doc_module(Name),

  [First, Second | _] = sumo:find_all(Name),
  First = sumo:find(Name, Module:id(First)),
  Second = sumo:find(Name, Module:id(Second)),
  notfound = sumo:find(Name, 0),
  ok.

-spec find_all(config()) -> ok.
find_all(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),

  [_, _, _, _, _, _, _, _] = sumo:find_all(Name),
  ok.

-spec find_by(config()) -> ok.
find_by(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_internal:get_doc_module(Name),

  Results = sumo:find_by(Name, [{last_name, <<"D">>}]),
  [_, _] = Results,
  SortFun = fun(A, B) -> Module:name(A) < Module:name(B) end,
  [First, Second | _] = lists:sort(SortFun, Results),

  {Today, _} = calendar:universal_time(),

  <<"B">> = Module:name(First),
  <<"D">> = Module:name(Second),
  3 = Module:age(First),
  4 = Module:age(Second),
  <<"D">> = Module:last_name(First),
  undefined = Module:address(First),
  Today = Module:birthdate(First),
  undefined = Module:height(First),
  undefined = Module:description(First),
  {Today, _} = Module:created_at(First),
  % Check that it returns what we have inserted
  [LastPerson | _NothingElse] = sumo:find_by(Name, [
    {last_name, <<"LastName">>}
  ]),
  <<"Name">> = Module:name(LastPerson),
  <<"LastName">> = Module:last_name(LastPerson),
  3 = Module:age(LastPerson),
  undefined = Module:address(LastPerson),
  {Date, _} = calendar:universal_time(),
  Date = Module:birthdate(LastPerson),
  1.75 = Module:height(LastPerson),
  <<"description">> = Module:description(LastPerson),
  <<"profile_image">> = Module:profile_image(LastPerson),
  <<"weird_field">> = Module:weird_field(LastPerson),
  {Today, _} = Module:created_at(LastPerson),

  %% Check find_by ID
  FirstId = Module:id(First),
  [First1] = sumo:find_by(Name, [{id, FirstId}]),
  [First1] = sumo:find_by(Name, [{last_name, <<"D">>}, {id, FirstId}]),
  [] = sumo:find_by(Name, [{name, <<"NotB">>}, {id, FirstId}]),
  First1 = First,
  %% Check pagination
  Results1 = sumo:find_by(Name, [], 3, 1),
  [_, _, _] = Results1,

  %% This test is #177 github issue related
  [_, _, _, _, _, _, _, _] = sumo:find_by(Name, []),
  Robot = sumo:find_by(Name, [{name, <<"Model T-2000">>}]),
  [_] = Robot,
  ok.

-spec delete_all(config()) -> ok.
delete_all(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),

  sumo:delete_all(Name),
  [] = sumo:find_all(Name),
  ok.

-spec delete(config()) -> ok.
delete(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_internal:get_doc_module(Name),

  %% delete_by
  2 = sumo:delete_by(Name, [{last_name, <<"D">>}]),
  Results = sumo:find_by(Name, [{last_name, <<"D">>}]),
  [] = Results,

  %% delete
  [First | _ ] = All = sumo:find_all(Name),
  Id = Module:id(First),
  sumo:delete(Name, Id),
  NewAll = sumo:find_all(Name),
  [_] = All -- NewAll,
  ok.

-spec check_proper_dates(config()) -> ok.
check_proper_dates(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_internal:get_doc_module(Name),

  [P0] = sumo:find_by(Name, [{name, <<"A">>}]),
  P1 = sumo:find(Name, Module:id(P0)),
  [P2 | _] = sumo:find_all(Name),

  {Date, _} = calendar:universal_time(),

  Date = Module:birthdate(P0),
  {Date, {_, _, _}} = Module:created_at(P0),
  Date = Module:birthdate(P1),
  {Date, {_, _, _}} = Module:created_at(P1),
  Date = Module:birthdate(P2),
  {Date, {_, _, _}} = Module:created_at(P2),

  Person = sumo:persist(Name, Module:new(<<"X">>, <<"Z">>, 6)),
  Date = Module:birthdate(Person),
  ok.

-spec init_store(atom()) -> ok.
init_store(Name) ->
  sumo:create_schema(Name),
  Module = sumo_internal:get_doc_module(Name),
  sumo:delete_all(Name),

  sumo:persist(Name, Module:new(<<"A">>, <<"E">>, 6)),
  sumo:persist(Name, Module:new(<<"B">>, <<"D">>, 3)),
  sumo:persist(Name, Module:new(<<"C">>, <<"C">>, 5)),
  sumo:persist(Name, Module:new(<<"D">>, <<"D">>, 4)),
  sumo:persist(Name, Module:new(<<"E">>, <<"A">>, 2)),
  sumo:persist(Name, Module:new(<<"F">>, <<"E">>, 1)),
  sumo:persist(Name, Module:new(<<"Model T-2000">>, <<"undefined">>, 7)),

  {Date, _} = calendar:universal_time(),
  sumo:persist(
    Name,
    Module:new(
      <<"Name">>,
      <<"LastName">>,
      3,
      undefined,
      Date,
      1.75,
      <<"description">>,
      <<"profile_image">>,
      <<"weird_field">>
    )
  ),
  ok.
