-module(sumo_events_test_helper).

%% Test Cases - Helpers
-export([events_manager_supervisor_running/1]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
%%%=============================================================================

-spec events_manager_supervisor_running(Config :: config()) -> ok.
events_manager_supervisor_running(Config) ->
  {_, DocName} = lists:keyfind(name, 1, Config),

  case sumo_config:get_event_manager(DocName) of
    undefined ->
      ok;
    EventManager ->
      ct:comment("~p should be running", [EventManager]),
      [EventManager] = gen_event:which_handlers(EventManager),
      ok
  end.
