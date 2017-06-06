-module(sumo_basic_test_helper).

%% Common Test Cases
-export([
  create_schema/1,
  find/1,
  find_all/1,
  find_by/1,
  delete_all/1,
  delete/1,
  check_proper_dates/1,
  count/1,
  persist_using_changeset/1
]).

%% Shared Helpers
-export([init_store/1]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
%%%=============================================================================

-spec create_schema(config()) -> ok.
create_schema(Config) ->
  ok = sumo:create_schema(),
  Tables = mnesia:system_info(tables),
  {_, Name} = lists:keyfind(name, 1, Config),
  true = lists:member(Name, Tables),
  ok.

-spec find(config()) -> ok.
find(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  [First, Second | _] = sumo:find_all(Name),
  First = sumo:find_one(Name, [{id, Module:id(First)}]),
  Second = sumo:fetch(Name, Module:id(Second)),
  notfound = sumo:fetch(Name, 0),
  notfound = sumo:find_one(Name, [{id, 0}]),
  ok.

-spec find_all(config()) -> ok.
find_all(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),

  [_, _, _, _, _, _, _, _] = sumo:find_all(Name),
  ok.

-spec find_by(config()) -> ok.
find_by(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

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
  false = Module:is_blocked(First),
  true = Module:weird_field1(First),
  undefined = Module:weird_field2(First),
  undefined = Module:weird_field3(First),

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
  true = Module:is_blocked(LastPerson),
  {mytuple, false, 1, "2", <<"3">>} = Module:weird_field1(LastPerson),
  [1, true, <<"hi">>, 1.1] = Module:weird_field2(LastPerson),
  #{a := 1,
    b := [1, "2", <<"3">>],
    <<"c">> := false
  } = Module:weird_field3(LastPerson),
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
  {EventId, Name, pre_delete_all, []} = pick_up_event(),
  {EventId, Name, deleted_all, []} = pick_up_event(),
  [] = sumo:find_all(Name),
  ok.

-spec delete(config()) -> ok.
delete(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  %% delete_by
  Conditions = [{last_name, <<"D">>}],
  2 = sumo:delete_by(Name, Conditions),

  {EventId, Name, pre_deleted_total, [Conditions]} = pick_up_event(),
  {EventId, Name, deleted_total, [2, Conditions]} = pick_up_event(),

  Results = sumo:find_by(Name, Conditions),
  [] = Results,

  %% delete
  [First | _ ] = All = sumo:find_all(Name),
  Id = Module:id(First),
  sumo:delete(Name, Id),

  % sumo:delete/2 uses internally sumo:delete_by/2, we handle those events too
  IdField = sumo_internal:id_field_name(Name),
  {EventId2, Name, pre_deleted, [Id]} = pick_up_event(),
  {EventId4, Name, pre_deleted_total, [[{IdField, Id}]]} = pick_up_event(),
  {EventId4, Name, deleted_total, [1, [{IdField, Id}]]} = pick_up_event(),
  {EventId2, Name, deleted, [Id]} = pick_up_event(),

  NewAll = sumo:find_all(Name),
  [_] = All -- NewAll,
  ok.

-spec check_proper_dates(config()) -> ok.
check_proper_dates(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  [P0] = sumo:find_by(Name, [{name, <<"A">>}]),
  P1 = sumo:fetch(Name, Module:id(P0)),
  [P2 | _] = sumo:find_all(Name),

  {Date, _} = calendar:universal_time(),

  Date = Module:birthdate(P0),
  {Date, {_, _, _}} = Module:created_at(P0),
  Date = Module:birthdate(P1),
  {Date, {_, _, _}} = Module:created_at(P1),
  Date = Module:birthdate(P2),
  {Date, {_, _, _}} = Module:created_at(P2),

  Person = create(Name, Module:new(<<"X">>, <<"Z">>, 6)),
  Date = Module:birthdate(Person),
  ok.

-spec count(config()) -> ok.
count(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),

  8 = length(sumo:find_all(Name)),
  8 = sumo:count(Name),

  _ = try sumo:count(wrong)
  catch
    _:no_workers -> ok
  end,

  Conditions = [{last_name, <<"D">>}],
  2 = sumo:delete_by(Name, Conditions),
  6 = sumo:count(Name),
  ok.

-spec persist_using_changeset(config()) -> ok.
persist_using_changeset(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  Module = sumo_config:get_prop_value(Name, module),

  [] = sumo:find_by(Name, [{name, <<"John">>}]),
  [P1] = sumo:find_by(Name, [{name, <<"A">>}]),

  Schema = Module:sumo_schema(),
  Allowed = [sumo_internal:field_name(F) || F <- sumo_internal:schema_fields(Schema)],
  CS1 = sumo_changeset:cast(people, P1, #{name => <<"John">>, age => 34}, Allowed),
  _ = sumo:persist(CS1),
  [P2] = sumo:find_by(Name, [{name, <<"John">>}]),
  <<"John">> = Module:name(P2),
  34 = Module:age(P2),

  CS2 = sumo_changeset:validate_number(CS1, age, [{less_than_or_equal_to, 33}]),
  {error, CS2} = sumo:persist(CS2),
  ok.

%%%=============================================================================
%%% Helpers
%%%=============================================================================

-spec init_store(atom()) -> ok.
init_store(Name) ->
  sumo:create_schema(Name),
  Module = sumo_config:get_prop_value(Name, module),
  sumo:delete_all(Name),

  DT = {Date, _} = calendar:universal_time(),

  create(Name, Module:new(<<"A">>, <<"E">>, 6)),
  create(Name, Module:from_map(#{
    name         => <<"B">>,
    last_name    => <<"D">>,
    age          => 3,
    birthdate    => Date,
    created_at   => DT,
    weird_field1 => true
  })),
  create(Name, Module:new(<<"C">>, <<"C">>, 5)),
  create(Name, Module:new(<<"D">>, <<"D">>, 4)),
  create(Name, Module:new(<<"E">>, <<"A">>, 2)),
  create(Name, Module:new(<<"F">>, <<"E">>, 1)),
  create(Name, Module:new(<<"Model T-2000">>, <<"undefined">>, 7)),

  create(Name, Module:from_map(#{
    name          => <<"Name">>,
    last_name     => <<"LastName">>,
    age           => 3,
    birthdate     => Date,
    created_at    => DT,
    height        => 1.75,
    description   => <<"description">>,
    profile_image => <<"profile_image">>,
    is_blocked    => true,
    weird_field1  => {mytuple, false, 1, "2", <<"3">>},
    weird_field2  => [1, true, <<"hi">>, 1.1],
    weird_field3  => #{a => 1, b => [1, "2", <<"3">>], <<"c">> => false}
  })),

  clean_events(),
  ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

pick_up_event() ->
  sumo_test_people_events_manager:pick_up_event().

clean_events() ->
  sumo_test_people_events_manager:clean_events().

create(Name, Args) ->
  clean_events(),
  Res = sumo:persist(Name, Args),
  {EventId, Name, pre_persisted, [Args]} = pick_up_event(),
  {EventId, Name, persisted, [Res]} = pick_up_event(),
  Res.
