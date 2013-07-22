%%% @doc This module is in charge of the event management.
%%% Copyright 2012 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
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
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(sumo_event).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%% Include standard types.
-include_lib("include/sumo_doc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([dispatch/2, dispatch/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Dispatchs an event through gen_event:notify/2.
-spec dispatch(sumo_schema_name(), term()) -> ok.
dispatch(DocName, Event) ->
  dispatch(DocName, Event, []).

%% @doc Dispatchs an event through gen_event:notify/2.
-spec dispatch(sumo_schema_name(), term(), term()) -> ok.
dispatch(DocName, Event, Args) ->
  case get_event_manager(DocName) of
    undefined -> ok;
    EventManager -> gen_event:notify(EventManager, {DocName, Event, Args})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the name of the event manager configured for the given
%% doc, or undefined.
-spec get_event_manager(
  sumo_schema_name()
) -> undefined|atom()|{atom(), term()}.
get_event_manager(DocName) ->
  {ok, Docs} = application:get_env(sumo, events),
  case Docs of
    undefined -> undefined;
    EventManagers -> case proplists:get_value(DocName, EventManagers) of
      undefined -> undefined;
      Name -> Name
    end
  end.
