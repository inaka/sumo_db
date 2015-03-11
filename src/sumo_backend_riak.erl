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
-export([get_connection/1, checkin_conn/2, checkout_conn/1]).

%%% Exports for sumo_backend
-export([start_link/2]).

%%% Exports for gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type r_host() :: iolist() | string().
-type r_port() :: non_neg_integer().
-type r_opts() :: [term()].
-type r_pool() :: [pid()].

-record(state, {conn_args      :: {r_host(), r_port(), r_opts()},
                conn_pool = [] :: r_pool()}).
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

-spec checkin_conn(atom() | pid(), pid()) -> atom().
checkin_conn(Name, Conn) ->
  gen_server:call(Name, {checkin_conn, Conn}).

-spec checkout_conn(atom() | pid()) -> atom().
checkout_conn(Name) ->
  gen_server:call(Name, checkout_conn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server stuff.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([term()]) -> {ok, state()}.
init(Options) ->
  %% Get connection parameters
  Host = proplists:get_value(host, Options, "127.0.0.1"),
  Port = proplists:get_value(port, Options, 8087),
  PoolSize = proplists:get_value(poolsize, Options, 10),
  Opts = riak_opts(Options),
  %% Create Riak connection pool
  F = fun(_E, Acc) ->
        {ok, Conn} = riakc_pb_socket:start_link(Host, Port, Opts),
        [Conn | Acc]
      end,
  ConnPool = lists:foldl(F, [], lists:seq(1, PoolSize)),
  %% Initial state
  {ok, #state{conn_args = {Host, Port, Opts}, conn_pool = ConnPool}}.

%% @todo: These are workarounds, a real connection pool needs to be added.
%% Workaround 1: when the store calls 'get_connection', a new Riak connection
%% is returned - one to one model (between connection and store).
%% Workaround 2: a simple list (LIFO) was added to hold a set of connections
%% that are created in the 'init' function (see code above). When the store
%% needs a connection, must call 'checkout_conn' to get the Pid, and when
%% it finish, must call 'checkin_conn'. These operations have some problems,
%% for instance, the don't consider an overflow, so the amount of new
%% connection are not watched, so in case of high concurrency, exist the
%% risk of have so many connection hitting the DB and causing contention.
-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_connection,
            _From,
            State = #state{conn_args = {Host, Port, Opts}}) ->
  {ok, Conn} = riakc_pb_socket:start_link(Host, Port, Opts),
  {reply, Conn, State};
handle_call({checkin_conn, Conn},
            _From,
            State = #state{conn_args = {Host, Port, Opts},
                           conn_pool = ConnPool}) ->
  NewConn = case is_process_alive(Conn) of
              true ->
                Conn;
              false ->
                {ok, Conn0} = riakc_pb_socket:start_link(Host, Port, Opts),
                Conn0
            end,
  {reply, ok, State#state{conn_pool = [NewConn | ConnPool]}};
handle_call(checkout_conn,
            _From,
            State = #state{conn_args = {Host, Port, Opts}, conn_pool = []}) ->
  {ok, Conn} = riakc_pb_socket:start_link(Host, Port, Opts),
  {reply, Conn, State#state{conn_pool = [Conn]}};
handle_call(checkout_conn,
            _From,
            State = #state{conn_args = {Host, Port, Opts},
                           conn_pool = ConnPool}) ->
  [Conn | _T] = ConnPool,
  NewConn = case is_process_alive(Conn) of
              true ->
                Conn;
              false ->
                {ok, Conn0} = riakc_pb_socket:start_link(Host, Port, Opts),
                Conn0
            end,
  {reply, NewConn, State#state{conn_pool = (ConnPool -- [Conn])}}.

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
