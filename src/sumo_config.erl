%%%-------------------------------------------------------------------
%%% @doc Main **internal** module for sumo.
%%%      Use this one from your own applications.
%%%
%%% Copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
%%% either express or implied. See the License for the specific
%%% language governing permissions and limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%-------------------------------------------------------------------
-module(sumo_config).

%% API
-export([
  init/0,
  get_docs/0,
  get_doc/1,
  get_store/1,
  get_props/1,
  get_prop_value/2,
  get_events/0,
  get_event_manager/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type doc_config() :: {DocName :: atom(), Store :: atom(), Props :: map()}.

-type event_config() :: {DocName :: atom(), EventHandler :: module()}.

-export_type([
  doc_config/0,
  event_config/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
  sumo_config = ets:new(sumo_config, [
    protected,
    named_table,
    {read_concurrency, true}
  ]),
  Docs = application:get_env(sumo_db, docs, []),
  Events = application:get_env(sumo_db, events, []),
  NewDocs = lists:foldl(fun({DocName, EventManager}, Acc) ->
    case lists:keyfind(DocName, 1, Acc) of
      {DocName, Store, Props} ->
        NewDoc = {DocName, Store, Props#{event_manager => EventManager}},
        lists:keyreplace(DocName, 1, Acc, NewDoc);
      _ ->
        Acc
    end
  end, Docs, Events),
  true = ets:insert(sumo_config, NewDocs),
  ok.

-spec get_docs() -> [doc_config()].
get_docs() ->
  application:get_env(sumo_db, docs, []).

-spec get_doc(atom()) -> doc_config() | undefined.
get_doc(DocName) ->
  case ets:lookup(?MODULE, DocName) of
    [Doc] -> Doc;
    []    -> undefined
  end.

-spec get_store(atom()) -> atom().
get_store(DocName) ->
  lookup_element(DocName, 2).

-spec get_props(atom()) -> map().
get_props(DocName) ->
  lookup_element(DocName, 3).

-spec get_prop_value(atom(), atom()) -> term().
get_prop_value(DocName, Prop) ->
  maps:get(Prop, get_props(DocName), undefined).

-spec get_events() -> [event_config()].
get_events() ->
  application:get_env(sumo_db, events, []).

-spec get_event_manager(atom()) -> module().
get_event_manager(DocName) ->
  get_prop_value(DocName, event_manager).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
lookup_element(Key, Pos) ->
  try ets:lookup_element(?MODULE, Key, Pos)
  catch
    _:_ -> undefined
  end.
