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

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  get_docs/0,
  get_doc/1,
  get_store/1,
  get_props/1,
  get_prop_value/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-type state() :: term().

-type doc_config() :: {DocName :: atom(), Store :: atom(), Props :: map()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
-spec init(Args :: term()) -> {ok, term()}.
init([]) ->
  Docs = application:get_env(sumo_db, docs, []),
  ?MODULE = ets:new(?MODULE, [
    public,
    named_table,
    {read_concurrency, true}
  ]),
  true = ets:insert(?MODULE, Docs),
  {ok, #{}}.

%% @hidden
-spec handle_call(term(), _, state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
lookup_element(Key, Pos) ->
  try ets:lookup_element(?MODULE, Key, Pos)
  catch
    _:_ -> undefined
  end.
