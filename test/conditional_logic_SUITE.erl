-module(conditional_logic_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([
  {sumo_conditionals_test_helper, [
    dates/1,
    backward_compatibility/1,
    or_conditional/1,
    and_conditional/1,
    not_null_conditional/1,
    null_conditional/1,
    operators/1,
    deeply_nested/1
  ]}
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
  sumo_conditionals_test_helper:init_store(people),
  [{name, people} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  _ = application:stop(sumo_db),
  Config.
