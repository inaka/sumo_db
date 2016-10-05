%%% @doc Main interface for stores.
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
-module(sumo_store).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behaviour(gen_server).

%%% Public API.
-export([
  start_link/3,
  create_schema/2,
  persist/2,
  fetch/3,
  delete_by/3,
  delete_all/2,
  find_all/2,
  find_all/5,
  find_by/3,
  find_by/5,
  find_by/6,
  count/2,
  call/4
]).

%%% gen_server callbacks
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

-record(state, {
  handler       = undefined :: module(),
  handler_state = undefined :: any()
}).

-type state() :: #state{}.

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-type result(R, S)    :: {ok, R, S} | {error, term(), S}.
-type result(S)       :: {ok, S} | {error, term(), S}.
-type affected_rows() :: unknown | non_neg_integer().

-export_type([result/2, result/1, affected_rows/0]).

-callback init(term()) -> {ok, term()}.

-callback create_schema(Schema, State) -> Res when
  Schema :: sumo:schema(),
  Res    :: result(State).

-callback persist(Doc, State) -> Res when
  Doc :: sumo_internal:doc(),
  Res :: result(sumo_internal:doc(), State).

-callback fetch(Schema, Id, State) -> Res when
  Schema :: sumo:schema_name(),
  Id     :: sumo:field_value(),
  Res    :: result(sumo_internal:doc(), State).

-callback delete_by(Schema, Conditions, State) -> Res when
  Schema     :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Res        :: result(affected_rows(), State).

-callback delete_all(Schema, State) -> Res when
  Schema :: sumo:schema_name(),
  Res    :: result(affected_rows(), State).

-callback find_by(Schema, Conditions, State) -> Res when
  Schema     :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Res        :: result([sumo_internal:doc()], State).

-callback find_by(Schema, Conditions, Limit, Offset, State) -> Res when
  Schema     :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: result([sumo_internal:doc()], State).

-callback find_by(Schema, Conditions, Sort, Limit, Offset, State) -> Res when
  Schema     :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Sort       :: sumo:sort(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: result([sumo_internal:doc()], State).

-callback find_all(Schema, State) -> Res when
  Schema :: sumo:schema_name(),
  Res    :: result([sumo_internal:doc()], State).

-callback find_all(Schema, Sort, Limit, Offset, State) -> Res when
  Schema :: sumo:schema_name(),
  Sort   :: sumo:sort(),
  Limit  :: non_neg_integer(),
  Offset :: non_neg_integer(),
  Res    :: result([sumo_internal:doc()], State).

-callback count(Schema, State) -> Res when
  Schema :: sumo:schema_name(),
  Res    :: result(non_neg_integer(), State).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Starts and links a new process for the given store implementation.
-spec start_link(atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Module, Options) ->
  PoolSize = sumo_utils:keyfind(workers, Options, 100),
  WPoolConfigOpts = application:get_env(sumo_db, wpool_opts, []),
  WPoolOptions = [
    {overrun_warning, 5000},
    {overrun_handler, {sumo_internal, report_overrun}},
    {workers, PoolSize},
    {worker, {?MODULE, [Module, Options]}}
  ],
  wpool:start_pool(Name, WPoolConfigOpts ++ WPoolOptions).

%% @doc Creates the schema of the given docs in the given store name.
-spec create_schema(atom(), sumo:schema()) -> ok | {error, term()}.
create_schema(Name, Schema) ->
  wpool:call(Name, {create_schema, Schema}).

%% @doc Persist the given doc with the given store name.
-spec persist(Name, Doc) -> Res when
  Name :: atom(),
  Doc  :: sumo_internal:doc(),
  Res  :: {ok, sumo_internal:doc()} | {error, term()}.
persist(Name, Doc) ->
  wpool:call(Name, {persist, Doc}).

%% @doc Fetch a single doc by its `Id'.
-spec fetch(Name, DocName, Id) -> Res when
  Name    :: atom(),
  DocName :: sumo:schema_name(),
  Id      :: sumo:field_value(),
  Res     :: {ok, sumo_internal:doc()} | {error, term()}.
fetch(Name, DocName, Id) ->
  wpool:call(Name, {fetch, DocName, Id}).

%% @doc Deletes the docs identified by the given conditions.
-spec delete_by(Name, DocName, Conditions) -> Res when
  Name       :: atom(),
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Res        :: {ok, non_neg_integer()} | {error, term()}.
delete_by(Name, DocName, Conditions) ->
  wpool:call(Name, {delete_by, DocName, Conditions}).

%% @doc Deletes all docs in the given store name.
-spec delete_all(Name, DocName) -> Res when
  Name    :: atom(),
  DocName :: sumo:schema_name(),
  Res     :: {ok, non_neg_integer()} | {error, term()}.
delete_all(Name, DocName) ->
  wpool:call(Name, {delete_all, DocName}).

%% @doc Returns all docs from the given store name.
-spec find_all(Name, DocName) -> Res when
  Name    :: atom(),
  DocName :: sumo:schema_name(),
  Res     :: {ok, [sumo_internal:doc()]} | {error, term()}.
find_all(Name, DocName) ->
  wpool:call(Name, {find_all, DocName}).

%% @doc
%% Returns Limit docs starting at Offset from the given store name,
%% ordered by OrderField. OrderField may be 'undefined'.
%% @end
-spec find_all(Name, DocName, SortFields, Limit, Offset) -> Res when
  Name       :: atom(),
  DocName    :: sumo:schema_name(),
  SortFields :: sumo:sort(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: {ok, [sumo_internal:doc()]} | {error, term()}.
find_all(Name, DocName, SortFields, Limit, Offset) ->
  wpool:call(Name, {find_all, DocName, SortFields, Limit, Offset}).

%% @doc
%% Finds documents that match the given conditions in the given
%% store name.
%% @end
-spec find_by(Name, DocName, Conditions) -> Res when
  Name       :: atom(),
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Res        :: {ok, [sumo_internal:doc()]} | {error, term()}.
find_by(Name, DocName, Conditions) ->
  wpool:call(Name, {find_by, DocName, Conditions}).

%% @doc
%% Finds documents that match the given conditions in the given
%% store name.
%% @end
-spec find_by(Name, DocName, Conditions, Limit, Offset) -> Res when
  Name       :: atom(),
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: {ok, [sumo_internal:doc()]} | {error, term()}.
find_by(Name, DocName, Conditions, Limit, Offset) ->
  wpool:call(Name, {find_by, DocName, Conditions, Limit, Offset}).

%% @doc
%% Finds documents that match the given conditions in the given
%% store name.
%% @end
-spec find_by(Name, DocName, Conditions, SortFields, Limit, Offset) -> Res when
  Name       :: atom(),
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  SortFields :: sumo:sort(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  Res        :: {ok, [sumo_internal:doc()]} | {error, term()}.
find_by(Name, DocName, Conditions, SortFields, Limit, Offset) ->
  wpool:call(Name, {find_by, DocName, Conditions, SortFields, Limit, Offset}).

%% @doc Counts the total number of docs in the given schema name `DocName'.
-spec count(Name, DocName) -> Res when
  Name    :: atom(),
  DocName :: sumo:schema_name(),
  Res     :: {ok, non_neg_integer()} | {error, term()}.
count(Name, DocName) ->
  wpool:call(Name, {count, DocName}).

%% @doc Calls a custom function in the given store name.
-spec call(Name, DocName, Function, Args) -> Res when
  Name     :: atom(),
  DocName  :: sumo:schema_name(),
  Function :: atom(),
  Args     :: [term()],
  Res      :: ok | {ok, term()} | {error, term()}.
call(Name, DocName, Function, Args) ->
  {ok, Timeout} = application:get_env(sumo_db, query_timeout),
  wpool:call(
    Name,
    {call, DocName, Function, Args},
    wpool:default_strategy(),
    Timeout).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%% @doc Called by start_link.
%% @hidden
-spec init([term()]) -> {ok, state()}.
init([Module, Options]) ->
  {ok, HState} = Module:init(Options),
  {ok, #state{handler=Module, handler_state=HState}}.

%% @doc handles calls.
%% @hidden
-spec handle_call(term(), _, state()) -> {reply, tuple(), state()}.
handle_call(
  {create_schema, Schema}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {Result, NewState} = case Handler:create_schema(Schema, HState) of
    {ok, NewState_} -> {ok, NewState_};
    {error, Error, NewState_} -> {{error, Error}, NewState_}
  end,
  {reply, Result, State#state{handler_state=NewState}};

handle_call(
  {persist, Doc}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:persist(Doc, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {fetch, DocName, Id}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:fetch(DocName, Id, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {delete_by, DocName, Conditions}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:delete_by(DocName, Conditions, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {delete_all, DocName}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:delete_all(DocName, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {find_all, DocName}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:find_all(DocName, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state = NewState}};

handle_call(
  {find_all, DocName, SortFields, Limit, Offset}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:find_all(
    DocName, SortFields, Limit, Offset, HState
  ),
  {reply, {OkOrError, Reply}, State#state{handler_state = NewState}};

handle_call(
  {find_by, DocName, Conditions}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:find_by(DocName, Conditions, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {find_by, DocName, Conditions, Limit, Offset}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:find_by(
    DocName, Conditions, Limit, Offset, HState
  ),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {find_by, DocName, Conditions, SortFields, Limit, Offset}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:find_by(
    DocName, Conditions, SortFields, Limit, Offset, HState
  ),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}};

handle_call(
  {count, DocName}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  {OkOrError, Reply, NewState} = Handler:count(DocName, HState),
  {reply, {OkOrError, Reply}, State#state{handler_state = NewState}};

handle_call(
  {call, DocName, Function, Args}, _From,
  #state{handler = Handler, handler_state = HState} = State
) ->
  RealArgs = lists:append(Args, [DocName, HState]),
  {OkOrError, Reply, NewState} = erlang:apply(Handler, Function, RealArgs),
  {reply, {OkOrError, Reply}, State#state{handler_state=NewState}}.

%% @hidden
-spec handle_cast(term(), state()) ->
  {noreply, state()} |
  {noreply, state(), non_neg_integer()} |
  {noreply, state(), hibernate} |
  {stop, term(), state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) ->
  {noreply, state()} |
  {noreply, state(), non_neg_integer()} |
  {noreply, state(), hibernate} |
  {stop, term(), state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
