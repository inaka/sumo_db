%%% @doc This module is in charge of the event management.
%%%
%%% Copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sumo_event).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%%% API
-export([dispatch/2, dispatch/3, dispatch/4]).

%%% Types
-type event_id() :: reference().

-export_type([event_id/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Dispatch an event through gen_event:notify/2.
-spec dispatch(sumo:schema_name(), term()) -> event_id() | no_event_managers.
dispatch(DocName, Event) ->
  dispatch(DocName, Event, []).

%% @doc Dispatch an event through gen_event:notify/2.
-spec dispatch(sumo:schema_name(), term(), term()) ->
  event_id() | no_event_managers.
dispatch(DocName, Event, Args) ->
  EventId = make_ref(),
  dispatch(DocName, EventId, Event, Args).

%% @doc Dispatch an event through gen_event:notify/2.
-spec dispatch(DocName, EventId, Event, Args) -> Res when
  DocName :: sumo:schema_name(),
  EventId :: event_id(),
  Event   :: term(),
  Args    :: term(),
  Res     :: event_id() | no_event_managers.
dispatch(DocName, EventId, Event, Args) ->
  case sumo_config:get_event_managers(DocName) of
    [] ->
      no_event_managers;
    EventManagers ->
      ok = lists:foreach(fun(EventManager) ->
        gen_event:notify(EventManager, {EventId, DocName, Event, Args})
      end, EventManagers),
      EventId
  end.
