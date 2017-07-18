-module(sumo_optional_callbacks_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-export([
  t_optional_callbacks/1
]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

%%%=============================================================================
%%% Common Test
%%%=============================================================================

%% @hidden
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%% @hidden
init_per_suite(Config) ->
  ok = sumo_test_utils:start_apps(),
  [{schemas, [people1, people2, people3]} | Config].

%% @hidden
end_per_suite(Config) ->
  _ = application:stop(sumo_db),
  Config.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

%% @hidden
t_optional_callbacks(Config) ->
  run_for_all_schemas(Config, fun t_optional_callbacks_/1).

%% @hidden
t_optional_callbacks_(SchemaName) ->
  _ = sumo:call(SchemaName, send_msg, [noreply]),
  _ = sumo:call(SchemaName, send_msg, [hibernate]),
  _ = sumo:call(SchemaName, send_msg, [timeout]),
  _ = sumo:call(SchemaName, send_msg, [stop]),
  _ = sumo:call(SchemaName, send_msg, [throw]),
  ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
run_for_all_schemas(Config, Fun) ->
  {_, Schemas} = lists:keyfind(schemas, 1, Config),
  lists:foreach(Fun, Schemas).
