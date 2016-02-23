-module(sumo_events_test_helper).

%% Test Cases - Helpers
-export([events_manager_supervisor_running/1]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
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
      [] = gen_event:which_handlers(EventManager),
      ok
  end.
