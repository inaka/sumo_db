-module(sumo_test_store1).

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
  ]}
]).

-export([send_msg/3]).

%% @hidden
send_msg(noreply, _DocName, State) ->
  {ok, {raw, send({noreply, State})}, State};
send_msg(hibernate, _DocName, State) ->
  {ok, {raw, send({noreply, State, hibernate})}, State};
send_msg(timeout, _DocName, State) ->
  {ok, {raw, send({noreply, State, 1})}, State};
send_msg(stop, DocName, State) ->
  {ok, {raw, send({stop, DocName, State})}, State};
send_msg(throw, _DocName, State) ->
  {ok, {raw, send(throw)}, State};
send_msg(terminate, _DocName, State) ->
  {ok, {raw, send(terminate)}, State}.

%% @private
send(Msg) ->
  self() ! Msg,
  ok.
