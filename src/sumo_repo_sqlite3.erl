%%% @doc SQLite3 repository implementation.
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
-module(sumo_repo_sqlite3).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-include_lib("include/sumo_doc.hrl").
-include_lib("sqlite3/include/sqlite3.hrl").

-behavior(sumo_repo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public API.
-export([
  init/1, create_schema/2, persist/2, find_by/3, find_by/5,
  delete/3, delete_by/3, delete_all/2, execute/2, execute/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {db::pid()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
persist(_Doc, State) ->
  {error, not_implemented, State}.

delete(_DocName, _Id, State) ->
  {error, not_implemented, State}.

delete_by(_DocName, _Conditions, State) ->
  {error, not_implemented, State}.

delete_all(_DocName, State) ->
  {error, not_implemented, State}.

find_by(_DocName, _Conditions, _Limit, _Offset, State) ->
  {error, not_implemented, State}.

find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

create_schema(_Schema, State) ->
  {ok, State}.

execute(_Query, _Args, State) ->
  {error, not_implemented, State}.

execute(Query, State) ->
  execute(Query, [], State).

init(_Options) ->
  {ok, #state{}}.
