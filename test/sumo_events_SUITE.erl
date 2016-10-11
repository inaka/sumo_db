-module(sumo_events_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([{sumo_events_test_helper, [events_manager_supervisor_running/1]}]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common Test
%%%=============================================================================

-spec all() -> [atom()].
all() -> [events_manager_supervisor_running].

-spec init_per_suite(Config::config()) -> config().
init_per_suite(Config) ->
  ok = sumo_test_utils:start_apps(),
  [{name, people} | Config].

-spec end_per_suite(Config::config()) -> config().
end_per_suite(Config) ->
  _ = application:stop(sumo_db),
  Config.
