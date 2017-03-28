-module(sumo_test_utils).
-author('elbrujohalcon@inaka.net').

-export([start_apps/0]).

-spec start_apps() -> ok.
start_apps() ->
  {ok, _} = application:ensure_all_started(sumo_db),
  init_events(),
  ok.

%% @private
init_events() ->
  lists:foreach(fun(EventManager) ->
    gen_event:add_handler(EventManager, EventManager, [])
  end, sumo_config:get_event_managers()).
