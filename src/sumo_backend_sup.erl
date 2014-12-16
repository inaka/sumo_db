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

-define(CLD(Name, Module, Options),
  { Name                                                           % Child Id
  , {Module, start_link, [Name, Options]}                          % Start Fun
  , permanent                                                      % Restart
  , 5000                                                           % Shutdown
  , worker                                                         % Type
  , [Module]                                                       % Modules
  }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1]).

-behaviour(supervisor).

-type init_result() ::
   {ok,
    {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
     [supervisor:child_spec()]
    }
   }
   | ignore.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> init_result().
init([]) ->
  {ok, Backends} = application:get_env(sumo_db, storage_backends),
  Children = lists:map(
    fun({Name, Module, Options}) -> ?CLD(Name, Module, Options) end,
    Backends
  ),
  {ok, { {one_for_one, 5, 10}, Children} }.
