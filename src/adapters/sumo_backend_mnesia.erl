%%% @hidden
%%% @doc Storage backend for Mnesia.
%%%      This module is useless, it's just here to fulfil the protocol.
%%%      You **can't** have multiple mnesia backends in one node.
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
-module(sumo_backend_mnesia).
-author("Brujo Benavides <elbrujohalcon@inaka.net>").

-behaviour(gen_server).
-behaviour(sumo_backend).

%%% Exports for sumo_backend
-export([start_link/2]).

%%% Exports for gen_server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(atom(), proplists:proplist()) -> {ok, pid()}|term().
start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, Options, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init([term()]) -> {ok, state()}.
init(_Options) ->
  Node = node(),
  _ = application:stop(mnesia),
  _ = case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  {ok, #{}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(Msg, _From, State) -> {reply, {unexpected_message, Msg}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
