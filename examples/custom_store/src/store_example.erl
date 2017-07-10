-module(store_example).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(application).

-export([count_by_city/1]).
-export([start/0, start/2, stop/0, stop/1]).

start() -> application:ensure_all_started(store_example).

stop() -> application:stop(store_example).

start(_Type, _Args) ->
  ok = sumo:create_schema(),
  store_example_sup:start_link().

stop(_State) ->
  ok.

count_by_city(City) ->
  sumo:call(people, count_by_city, [City]).
