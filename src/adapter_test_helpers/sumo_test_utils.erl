-module(sumo_test_utils).
-author('elbrujohalcon@inaka.net').

-export([start_apps/0]).

-spec start_apps() -> ok.
start_apps() ->
  mnesia:create_schema([node()]),
  {ok, _} = application:ensure_all_started(mnesia),
  {ok, _} = application:ensure_all_started(sumo_db).
