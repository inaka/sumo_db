%%% @hidden
%%% @doc Riak storage backend implementation.
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
-module(sumo_backend_riak).
-author("Carlos Andres Bolanos <candres.bolanos@inakanetworks.com>").
-license("Apache License 2.0").

-behaviour(gen_server).
-behaviour(sumo_backend).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Public API.
-export(
  [ get_connection/1,
    get_state/1
  ]).

%%% Exports for sumo_backend
-export(
  [ start_link/2
  ]).

%%% Exports for gen_server
-export(
  [ init/1
  , handle_call/3
  , handle_cast/2
  , handle_info/2
  , terminate/2
  , code_change/3
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {conn :: pid(), bucket :: binary(), index :: binary()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(atom(), proplists:proplist()) -> {ok, pid()}|term().
start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, Options, []).

-spec get_connection(atom() | pid()) -> atom().
get_connection(Name) ->
  gen_server:call(Name, get_connection).

-spec get_state(atom() | pid()) -> state().
get_state(Name) ->
  gen_server:call(Name, get_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server stuff.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([term()]) -> {ok, state()}.
init(Options) ->
  %% Get connection parameters
  Host = proplists:get_value(host, Options, "127.0.0.1"),
  Port = proplists:get_value(port, Options, 8087),
  Opts = riak_opts(Options),
  BucketType = iolist_to_binary(
    proplists:get_value(bucket_type, Options)),
  Bucket = iolist_to_binary(
    proplists:get_value(bucket, Options, <<"sumo_test">>)),
  Index = iolist_to_binary(
    proplists:get_value(index, Options, <<"sumo_test_index">>)),
  %% Encoding
  %%Encoding = proplists:get_value(encoding, Options, json),
  %% Place Riak connection
  {ok, Conn} = riakc_pb_socket:start_link(Host, Port, Opts),
  %% Initial state
  {ok, #state{conn = Conn, bucket = {BucketType, Bucket}, index = Index}}.

-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_connection, _From, State = #state{conn = Conn}) ->
  {reply, Conn, State};
handle_call(get_state, _From, State) ->
  {reply, State, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server stuff.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec riak_opts([term()]) -> [term()].
riak_opts(Options) ->
  User = proplists:get_value(username, Options),
  Pass = proplists:get_value(password, Options),
  Opts0 = case User /= undefined andalso Pass /= undefined of
            true -> [{credentials, User, Pass}];
            _    -> []
          end,
  Opts1 = case lists:keyfind(connect_timeout, 1, Options) of
            {_, V1} -> [{connect_timeout, V1}, {auto_reconnect, true}] ++ Opts0;
            _       -> [{auto_reconnect, true}] ++ Opts0
          end,
  Opts1.
