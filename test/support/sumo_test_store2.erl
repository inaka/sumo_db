-module(sumo_test_store2).

-behaviour(sumo_store).

%% @todo remove this once mixer migrates specs better
-dialyzer([no_behaviours]).

-include_lib("mixer/include/mixer.hrl").
-mixin([
  {sumo_store_mnesia, [
    init/1,
    create_schema/2,
    persist/2,
    fetch/3,
    delete_by/3,
    delete_all/2,
    find_all/2, find_all/5,
    find_by/3, find_by/5, find_by/6,
    count/2,
    count_by/3
  ]},
  {sumo_test_store1, [
    send_msg/3
  ]}
]).

-export([handle_info/2, terminate/2]).

%% @hidden
handle_info(throw, State) ->
  throw({stop, throw, State});
handle_info(Info, _State) ->
  Info.

%% @hidden
terminate(throw, _State) ->
  throw(ok);
terminate(_Info, _State) ->
  ok.
