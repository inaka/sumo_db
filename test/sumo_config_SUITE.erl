-module(sumo_config_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2
]).

%% Test Cases
-export([
  t_basic_ops/1,
  t_add_remove_event_managers/1,
  t_reload_sumo_config/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  init_per_testcase
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
  ok = sumo_test_utils:start_apps(),
  Docs = application:get_env(sumo_db, docs, []),
  Events = application:get_env(sumo_db, events, []),
  [{docs, Docs}, {events, Events} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  cleanup_config(Config),
  _ = application:stop(sumo_db),
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  setup_config(),
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec t_basic_ops(config()) -> ok.
t_basic_ops(_Config) ->
  validate_docs(),

  % expected doc managers
  ExpectedManagersDoc1 = [
    ev_manager1,
    super_ev_manager
  ],
  ExpectedManagersDoc2 = [
    ev_manager1,
    ev_manager2,
    super_ev_manager
  ],
  ExpectedManagersDoc3 = [
    super_ev_manager
  ],

  % check event managers
  ExpectedManagersDoc1 = sumo_config:get_event_managers(doc1),
  ExpectedManagersDoc2 = sumo_config:get_event_managers(doc2),
  ExpectedManagersDoc3 = sumo_config:get_event_managers(doc3),
  ExpectedManagersDoc3 = sumo_config:get_event_managers(doc4),

  AllEvManagers = lists:usort([ev_manager1, ev_manager2, super_ev_manager]),
  AllEvManagers = sumo_config:get_event_managers(),

  ok.

-spec t_add_remove_event_managers(config()) -> ok.
t_add_remove_event_managers(_Config) ->
  validate_docs(),

  % expected doc managers
  ExpectedManagersDoc1 = [
    ev_manager1,
    super_ev_manager
  ],
  ExpectedManagersDoc3 = [
    super_ev_manager
  ],

  AllEvManagers = lists:usort([ev_manager1, ev_manager2, super_ev_manager]),
  AllEvManagers = sumo_config:get_event_managers(),

  % add event managers
  ExpectedManagersDoc11 = lists:usort([ev_managerA | ExpectedManagersDoc1]),
  ExpectedManagersDoc11 = sumo_config:add_event_managers(doc1, ev_managerA),
  ExpectedManagersDoc31 = lists:usort(
    [ev_managerA, ev_managerB | ExpectedManagersDoc3]),
  ExpectedManagersDoc31 = sumo_config:add_event_managers(
    doc3, [ev_managerA, ev_managerB]),

  AllEvManagers1 = lists:usort([ev_managerA, ev_managerB] ++ AllEvManagers),
  AllEvManagers1 = sumo_config:get_event_managers(),

  % remove event managers
  ExpectedManagersDoc11 = lists:usort([ev_managerA | ExpectedManagersDoc1]),
  ExpectedManagersDoc1 = sumo_config:remove_event_managers(doc1, ev_managerA),
  ExpectedManagersDoc31 = lists:usort(
    [ev_managerA, ev_managerB | ExpectedManagersDoc3]),
  ExpectedManagersDoc3 = sumo_config:remove_event_managers(
    doc3, [ev_managerA, ev_managerB]),

  AllEvManagers = sumo_config:get_event_managers(),

  ok.

-spec t_reload_sumo_config(config()) -> ok.
t_reload_sumo_config(_Config) ->
  validate_docs(),

  % set other manager and reload it
  NewDocs = [{docX, storeX, #{module => mod1}} | docs()],
  application:set_env(sumo_db, docs, NewDocs),
  application:set_env(sumo_db, events, [
    {docX, ev_managerX},
    {'_', super_ev_manager}
  ]),
  sumo_config:init(),

  % expected doc managers
  ExpectedManagersDoc1 = [
    ev_managerX,
    super_ev_manager
  ],
  ExpectedManagersDoc2 = [
    super_ev_manager
  ],

  % check event managers
  ExpectedManagersDoc2 = sumo_config:get_event_managers(doc1),
  ExpectedManagersDoc2 = sumo_config:get_event_managers(doc2),
  ExpectedManagersDoc2 = sumo_config:get_event_managers(doc3),
  ExpectedManagersDoc2 = sumo_config:get_event_managers(doc4),
  ExpectedManagersDoc1 = sumo_config:get_event_managers(docX),

  ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
setup_config() ->
  ok = application:set_env(sumo_db, docs, docs()),
  ok = application:set_env(sumo_db, events, events()),
  sumo_config:init().

%% @private
cleanup_config(Config) ->
  Docs = sumo_utils:keyfind(docs, Config),
  Events = sumo_utils:keyfind(events, Config),
  ok = application:set_env(sumo_db, docs, Docs),
  ok = application:set_env(sumo_db, events, Events).

%% @private
docs() ->
  [{doc1, store1, #{module => mod1}},
   {doc2, store2, #{module => mod2}},
   {doc3, store3, #{module => mod3}},
   {doc4, store4, #{module => mod4}}].

%% @private
events() ->
  [{doc1, ev_manager1},
   {doc2, [ev_manager1, ev_manager2]},
   {doc4, []},
   {'_', super_ev_manager}].

%% @private
validate_docs() ->
  Docs = lists:usort([DocName || {DocName, _, _} <- docs()]),
  Docs = lists:usort([DocName || {DocName, _, _} <- sumo_config:get_docs()]),
  ok.
