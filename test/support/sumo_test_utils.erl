-module(sumo_test_utils).
-author('elbrujohalcon@inaka.net').

-export([
  start_apps/0,
  assert_error/2
]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_apps() -> ok.
start_apps() ->
  {ok, _} = application:ensure_all_started(sumo_db),
  init_events(),
  ok.

-spec assert_error(term(), fun()) -> ok | no_return().
assert_error(Error, Fun) ->
  try Fun()
  catch
    _:Error -> ok
  end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
init_events() ->
  lists:foreach(fun(EventManager) ->
    gen_event:add_handler(EventManager, EventManager, [])
  end, sumo_config:get_event_managers()).
