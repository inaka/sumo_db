-module(sumo_changeset_SUITE).

-compile({parse_transform, fancyflow_trans}).

-include_lib("common_test/include/ct.hrl").

-import(sumo_test_utils, [assert_error/2]).

%% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_add_error/1,
  t_cast/1,
  t_change/1,
  t_put_change/1,
  t_get_change/1,
  t_delete_change/1,
  t_apply_changes/1,
  t_validate_change/1,
  t_validate_required/1,
  t_validate_inclusion/1,
  t_validate_number/1,
  t_validate_length/1,
  t_validate_format/1,
  t_nested_changeset_validations/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

-define(ALLOWED, [
  id,
  name,
  last_name,
  age,
  address,
  height,
  description,
  status,
  birthdate
]).

-define(REQUIRED, [id, name, last_name, age]).

-define(PERSON, #{
  id         => 1,
  last_name  => <<"other">>,
  age        => 33,
  height     => 1.85,
  birthdate  => <<"1980-09-22">>,
  created_at => {{2012, 2, 16}, {1, 6, 48}},
  is_blocked => false,
  status     => "active"
}).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% CT
%%%=============================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sumo_db),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  _ = application:stop(sumo_db),
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec t_add_error(config()) -> ok.
t_add_error(_Config) ->
  %% run changeset pipeline adding an error
  CS = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), ?PERSON, ?ALLOWED),
    sumo_changeset:add_error(_, status, <<"Invalid">>)),

  %% validate errors
  false = sumo_changeset:is_valid(CS),
  1 = length(sumo_changeset:errors(CS)),
  _ = validate_cs_errors(CS, [status]),

  ok.

-spec t_cast(config()) -> ok.
t_cast(_Config) ->
  %% create a person doc
  Person = default_person_doc(),
  PersonModel = sumo_internal:doc_fields(sumo_internal:from_user_doc(people, Person)),

  %% create params to be cast adding some intentional errors
  Params = ?PERSON#{age => '33', missing => 1},
  Allowed = [missing | ?ALLOWED],

  %% run changeset pipeline
  ExpectedChanges = #{
    birthdate => {{1980, 9, 22}, {0, 0, 0}},
    height    => 1.85,
    id        => 1,
    last_name => <<"other">>,
    status    => <<"active">>
  },
  CS = sumo_changeset:cast(people, Person, Params, Allowed),
  _ = validate_cs(CS, #{
    schema   => people,
    store    => sumo_test_mnesia,
    data     => PersonModel,
    params   => maps:with(Allowed, Params),
    changes  => ExpectedChanges,
    types    => {true, fun(M) -> maps:size(M) > 0 end},
    required => {[], fun(L) -> L end}
  }),

  %% validate errors
  false = sumo_changeset:is_valid(CS),
  1 = length(sumo_changeset:errors(CS)),
  _ = validate_cs_errors(CS, [age]),

  CS1 = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:cast(_, #{last_name => <<"other">>}, Allowed)),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  ok.

-spec t_change(config()) -> ok.
t_change(_Config) ->
  CS1 = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:change(_, #{last_name => <<"other">>})),
  1 = maps:size(sumo_changeset:changes(CS1)),

  CS2 = [pipe](people,
    sumo_changeset:change(_, default_person_doc(), #{last_name => <<"Darwin">>}),
    sumo_changeset:change(_, #{last_name => <<"other">>})),
  1 = maps:size(sumo_changeset:changes(CS2)),

  CS3 = [pipe](people,
    sumo_changeset:change(_, default_person_doc(), #{last_name => <<"Darwin">>}),
    sumo_changeset:cast(_, #{}, ?ALLOWED)),
  1 = maps:size(sumo_changeset:changes(CS3)),

  ok.

-spec t_put_change(config()) -> ok.
t_put_change(_Config) ->
  #{last_name := <<"other">>} = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"other">>),
    sumo_changeset:changes(_)),

  0 = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"Doe">>),
    sumo_changeset:changes(_),
    maps:size(_)),

  0 = [pipe](people,
    sumo_changeset:cast(_, sumo_test_people:new(<<"other">>, <<"other">>), #{}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"other">>),
    sumo_changeset:cast(_, #{last_name => <<"other">>}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"other">>),
    sumo_changeset:changes(_),
    maps:size(_)),

  ok.

-spec t_get_change(config()) -> ok.
t_get_change(_Config) ->
  CS1 = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"other">>)),
  1 = maps:size(sumo_changeset:changes(CS1)),

  <<"other">> = sumo_changeset:get_change(CS1, last_name),
  undefined = sumo_changeset:get_change(CS1, name),
  <<"default">> = sumo_changeset:get_change(CS1, name, <<"default">>),

  ok.

-spec t_delete_change(config()) -> ok.
t_delete_change(_Config) ->
  CS1 = [pipe](people,
    sumo_changeset:cast(_, default_person_doc(), #{}, ?ALLOWED),
    sumo_changeset:put_change(_, last_name, <<"other">>)),
  1 = maps:size(sumo_changeset:changes(CS1)),

  CS2 = sumo_changeset:delete_change(CS1, last_name),
  0 = maps:size(sumo_changeset:changes(CS2)),

  ok.

-spec t_apply_changes(config()) -> ok.
t_apply_changes(_Config) ->
  %% create a person doc
  Person = default_person_doc(),
  PersonModel = sumo_internal:doc_fields(sumo_internal:from_user_doc(people, Person)),

  %% run changeset pipeline
  CS1 = sumo_changeset:cast(people, Person, ?PERSON#{missing => 1}, ?ALLOWED),
  Data = sumo_changeset:data(CS1),
  true = Data == PersonModel,
  undefined = sumo_test_people:id(Person),
  <<"Doe">> = sumo_test_people:last_name(Person),
  undefined = sumo_test_people:age(Person),

  %% apply changes
  NewData = sumo_changeset:apply_changes(CS1),
  false = NewData == PersonModel,
  NewPerson = sumo_test_people:sumo_wakeup(NewData),
  1 = sumo_test_people:id(NewPerson),
  <<"other">> = sumo_test_people:last_name(NewPerson),
  33 = sumo_test_people:age(NewPerson),

  %% run changeset pipeline
  CS2 = sumo_changeset:cast(people, Person, #{}, ?ALLOWED),
  0 = maps:size(sumo_changeset:changes(CS2)),
  PersonModel = sumo_changeset:apply_changes(CS2),

  %% run changeset pipeline
  CS3 = [pipe](people,
    sumo_changeset:cast(_, Person, #{}, ?ALLOWED),
    sumo_changeset:put_change(_, missing, 2)
  ),
  1 = maps:size(sumo_changeset:changes(CS3)),
  PersonModel = sumo_changeset:apply_changes(CS3),

  ok.

-spec t_validate_change(config()) -> ok.
t_validate_change(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_change(_, age, fun(age, Age) ->
      case Age > 30 of
        true  -> [{age, <<"cannot be greater than 30">>}];
        false -> []
      end
    end)),

  %% validate errors
  false = sumo_changeset:is_valid(CS1),
  [{age, {<<"cannot be greater than 30">>, []}}] = sumo_changeset:errors(CS1),

  ok.

-spec t_validate_required(config()) -> ok.
t_validate_required(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED)),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON#{age => nil}, ?ALLOWED),
    sumo_changeset:validate_required(_, [address | ?REQUIRED])),

  %% validate errors
  false = sumo_changeset:is_valid(CS2),
  2 = length(sumo_changeset:errors(CS2)),
  _ = validate_cs_errors(CS2, [address, age]),

  %% should fails
  _ = assert_error({badarg, invalid}, fun() ->
    [pipe](people,
      sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
      sumo_changeset:validate_required(_, [invalid | ?REQUIRED]))
  end),

  ok.

-spec t_validate_inclusion(config()) -> ok.
t_validate_inclusion(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% valid statuses
  Statuses = [<<"active">>, <<"blocked">>],

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED),
    sumo_changeset:validate_inclusion(_, status, Statuses)),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON#{status => <<"invalid">>}, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED),
    sumo_changeset:validate_inclusion(_, status, Statuses)),

  %% validate errors
  false = sumo_changeset:is_valid(CS2),
  1 = length(sumo_changeset:errors(CS2)),
  _ = validate_cs_errors(CS2, [status]),

  ok.

-spec t_validate_number(config()) -> ok.
t_validate_number(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_number(_, age, [
      {less_than, 34},
      {less_than_or_equal_to, 33},
      {greater_than, 32},
      {greater_than_or_equal_to, 33},
      {equal_to, 33}
    ])),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  ValidationSet = [
    [{less_than, 30}],
    [{less_than_or_equal_to, 30}],
    [{greater_than, 40}],
    [{greater_than_or_equal_to, 40}],
    [{equal_to, 30}],
    [{less_than, 30}, {equal_to, 30}]
  ],
  _ = lists:foreach(fun(Validations) ->
    %% run changeset pipeline
    CS = [pipe](people,
      sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
      sumo_changeset:validate_number(_, age, Validations)),

    %% validate errors
    false = sumo_changeset:is_valid(CS),
    1 = length(sumo_changeset:errors(CS)),
    _ = validate_cs_errors(CS, [age])
  end, ValidationSet),

  %% should fails
  _ = assert_error({badarg, invalid_validation}, fun() ->
    [pipe](people,
      sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
      sumo_changeset:validate_number(_, age, [{invalid_validation, 33}]))
  end),

  ok.

-spec t_validate_length(config()) -> ok.
t_validate_length(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_length(_, last_name, [{is, 5}, {min, 2}, {max, 10}])),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  ValidationSet = [
    [{is, 3}],
    [{min, 10}],
    [{max, 3}],
    [{is, 5}, {min, 2}, {max, 3}]
  ],
  _ = lists:foreach(fun(Validations) ->
    %% run changeset pipeline
    CS = [pipe](people,
      sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
      sumo_changeset:validate_length(_, last_name, Validations)),

    %% validate errors
    false = sumo_changeset:is_valid(CS),
    [{last_name, {_, [{validation, length}]}}] = sumo_changeset:errors(CS)
  end, ValidationSet),

  ok.

-spec t_validate_format(config()) -> ok.
t_validate_format(_Config) ->
  %% create a person doc
  Person = default_person_doc(),

  %% run changeset pipeline
  CS1 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED),
    sumo_changeset:validate_format(_, last_name, <<"^oth">>)),

  %% validate errors
  true = sumo_changeset:is_valid(CS1),
  0 = length(sumo_changeset:errors(CS1)),

  %% run changeset pipeline
  CS2 = [pipe](people,
    sumo_changeset:cast(_, Person, ?PERSON, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED),
    sumo_changeset:validate_format(_, last_name, <<"^Doe">>)),

  %% validate errors
  false = sumo_changeset:is_valid(CS2),
  [{last_name, {<<"has invalid format">>, [{validation, format}]}}] = sumo_changeset:errors(CS2),

  ok.

-spec t_nested_changeset_validations(config()) -> ok.
t_nested_changeset_validations(_Config) ->
  Person = sumo_test_people:new(<<"John">>, <<"Doe">>),
  Params = #{age => 33, id => 1, <<"last_name">> => <<"other">>},

  _ = [pipe](people,
    sumo_changeset:cast(_, Person, Params, ?ALLOWED),
    sumo_changeset:validate_required(_, ?REQUIRED),
    sumo_changeset:validate_inclusion(_, status, [<<"active">>, <<"blocked">>]),
    sumo_changeset:validate_number(_, age, [{less_than_or_equal_to, 33}]),
    sumo_changeset:validate_length(_, last_name, [{min, 3}]),
    sumo_changeset:validate_format(_, last_name, <<"^oth">>)),

  ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
default_person_doc() ->
  sumo_test_people:new(<<"John">>, <<"Doe">>).

%% @private
validate_cs(CS, ParamsToCheck) ->
  maps:fold(fun
    (K, {Expected, Fun}, _Acc) when is_function(Fun) ->
      Expected = Fun(sumo_changeset:K(CS));
    (K, V, _Acc) ->
      V = sumo_changeset:K(CS)
  end, ok, ParamsToCheck).

%% @private
validate_cs_errors(CS, ErrorKeys) ->
  Errors = sumo_changeset:errors(CS),
  [true = sumo_utils:is_key(K, Errors) || K <- ErrorKeys].
