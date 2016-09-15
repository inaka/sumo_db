-module(sumo_events_test_helper).

%% Test Cases - Helpers
-export([events_manager_supervisor_running/1]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Test Cases - Helpers
%%%=============================================================================

-spec events_manager_supervisor_running(Config :: config()) -> ok.
events_manager_supervisor_running(Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),

  Events = application:get_env(sumo_db, events, []),
  case lists:keyfind(Name, 1, Events) of
    false ->
      ok;
    {Name, EventManager} ->
      ct:comment("~p should be running", [EventManager]),
      [] = gen_event:which_handlers(EventManager),
      ok
  end.
