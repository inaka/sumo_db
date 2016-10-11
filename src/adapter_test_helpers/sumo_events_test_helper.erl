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
  case sumo_config:get_event_managers(DocName) of
    [] ->
      ok;
    EventManagers ->
      ct:comment("~p should be running", [EventManagers]),
      EventManagers =
        [hd(gen_event:which_handlers(EM)) || EM <- EventManagers],
      ok
  end.
