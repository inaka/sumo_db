-module(sumo_events_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2
        ]).

-export([events_manager_supervisor_running/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  ExcludedFunctions = [module_info,
                       all,
                       test,
                       init_per_suite,
                       init_per_testcase,
                       end_per_suite,
                       find_by_sort,
                       find_all_sort
                      ],
  [F || {F, _} <- Exports, not lists:member(F, ExcludedFunctions)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),
  Config.

init_per_testcase(_, Config) ->
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec events_manager_supervisor_running(Config::config()) -> {comment, string()}.
events_manager_supervisor_running(_Config) ->
  Events = application:get_env(sumo_db, events, []),
  Fun = fun(Module) ->
    case lists:keyfind(Module, 1, Events) of
      false -> ok;
      {Module, EventManager} ->
        ct:comment("~p should be running", [EventManager]),
        [] = gen_event:which_handlers(EventManager)
    end
  end,
  lists:foreach(Fun, sumo_test_utils:all_people()),

  {comment, ""}.





