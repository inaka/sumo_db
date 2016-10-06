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
  get_prop_value/3,
  get_events/0,
  get_event_managers/0,
  get_event_managers/1,
  add_event_managers/2
]).

-include_lib("stdlib/include/ms_transform.hrl").

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
  case ets:info(?MODULE) of
    undefined ->
      do_init();
    _ ->
      true = ets:delete(?MODULE),
      do_init()
  end.

-spec get_docs() -> [doc_config()].
get_docs() ->
  MS = ets:fun2ms(fun({X, Y, Z}) -> {X, Y, Z} end),
  ets:select(?MODULE, MS).

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
  get_prop_value(DocName, Prop, undefined).

-spec get_prop_value(atom(), atom(), term()) -> term().
get_prop_value(DocName, Prop, Default) ->
  maps:get(Prop, get_props(DocName), Default).

-spec get_events() -> [event_config()].
get_events() ->
  application:get_env(sumo_db, events, []).

-spec get_event_managers() -> [module()].
get_event_managers() ->
  lookup_element('$event_managers', 2, []).

-spec get_event_managers(atom()) -> [module()].
get_event_managers(DocName) ->
  get_prop_value(DocName, event_managers, []).

-spec add_event_managers(atom(), [module()] | module()) -> [doc_config()].
add_event_managers(DocName, EventManagers) ->
  update_doc_entry(DocName, EventManagers, get_docs()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
do_init() ->
  ?MODULE = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  Docs = application:get_env(sumo_db, docs, []),
  Events = application:get_env(sumo_db, events, []),
  UpdatedDocs = load_entry(Docs, Events),
  EvManagers = load_event_managers(UpdatedDocs),
  Entries = [{'$event_managers', EvManagers} | UpdatedDocs],
  true = ets:insert(?MODULE, Entries),
  ok.

%% @private
load_entry(Docs, Events) ->
  lists:foldl(fun({DocName, EventManagers}, DocAcc) ->
    update_doc_entry(DocName, EventManagers, DocAcc)
  end, Docs, Events).

%% @private
load_event_managers(Docs) ->
  lists:usort(lists:foldl(fun({_, _, Props}, Acc) ->
    maps:get(event_managers, Props, []) ++ Acc
  end, [], Docs)).

%% @private
update_doc_entry(_DocName, [], Docs) ->
  Docs;
update_doc_entry('_', EventManagers, Docs) when is_list(EventManagers) ->
  DocNames = [DocName || {DocName, _, _} <- Docs],
  lists:foldl(fun(DocName, DocAcc) ->
    update_doc_entry(DocName, EventManagers, DocAcc)
  end, Docs, DocNames);
update_doc_entry(DocName, EventManagers, Docs) when is_list(EventManagers) ->
  case lists:keyfind(DocName, 1, Docs) of
    {DocName, Store, Props} ->
      CurrentEventManagers = maps:get(event_managers, Props, []),
      NewEventManagers = lists:usort(EventManagers ++ CurrentEventManagers),
      NewDoc = {DocName, Store, Props#{event_managers => NewEventManagers}},
      lists:keyreplace(DocName, 1, Docs, NewDoc);
    _ ->
      Docs
  end;
update_doc_entry(DocName, EventManager, Docs) when is_atom(EventManager) ->
  update_doc_entry(DocName, [EventManager], Docs).

%% @private
lookup_element(Key, Pos) ->
  lookup_element(Key, Pos, undefined).

%% @private
lookup_element(Key, Pos, Default) ->
  try ets:lookup_element(?MODULE, Key, Pos)
  catch
    _:_ -> Default
  end.
