-module(sumo_events_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([events_manager_supervisor_running/1]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common Test
%%%=============================================================================

-spec all() -> [atom()].
all() -> [events_manager_supervisor_running].

-spec init_per_suite(Config::config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),
  [{module, sumo_test_people_mnesia} | Config].

-spec end_per_suite(Config::config()) -> config().
end_per_suite(Config) ->
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec events_manager_supervisor_running(Config :: config()) -> ok.
events_manager_supervisor_running(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  Events = application:get_env(sumo_db, events, []),
  case lists:keyfind(Module, 1, Events) of
    false ->
      ok;
    {Module, EventManager} ->
      ct:comment("~p should be running", [EventManager]),
      [] = gen_event:which_handlers(EventManager)
  end.
