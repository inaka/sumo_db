%%% @hidden
%%% @doc Backend supervisor.
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
-module(sumo_backend_sup).
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behaviour(supervisor).

%%% API
-export([start_link/0]).

%%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%% hidden
init([]) ->
  {ok, Backends} = application:get_env(sumo_db, storage_backends),
  Children = lists:map(fun({Name, Module, Options}) ->
    child_spec(Name, Module, Options)
  end, Backends),
  {ok, {{one_for_one, 5, 10}, Children}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private
child_spec(Name, Module, Options) ->
  {Name,
   {Module, start_link, [Name, Options]},
   permanent,
   5000,
   worker,
   [Module]}.
