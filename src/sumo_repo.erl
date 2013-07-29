%%% @doc Main interface for repositories.
%%%
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
-module(sumo_repo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([behaviour_info/1]).

-include_lib("include/sumo_doc.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([
  create_schema/2, persist/2, find_by/3, find_by/5,
  delete/3, delete_all/2, call/4
]).
-export([start_link/3]).

%%% Exports for gen_server
-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {handler, handler_state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns all behavior callbacks.
-spec behaviour_info(callbacks) -> proplists:proplist()|undefined.
behaviour_info(callbacks) ->
  [
    {init,1}, {persist,2}, {delete,3}, {find_by,3}, {find_by,5},
    {create_schema,2}
  ];

behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Persist the given doc with the given repository name.
-spec persist(atom(), #sumo_doc{}) -> #sumo_doc{}.
persist(Name, #sumo_doc{}=Doc) ->
  gen_server:call(Name, {persist, Doc}).

%% @doc Deletes the doc identified by id in the given repository name.
-spec delete(atom(), sumo_schema_name(), term()) -> ok.
delete(Name, DocName, Id) ->
  gen_server:call(Name, {delete, DocName, Id}).

%% @doc Deletes all docs in the given repository name.
-spec delete_all(atom(), sumo_schema_name()) -> ok.
delete_all(Name, DocName) ->
  gen_server:call(Name, {delete_all, DocName}).

%% @doc Finds documents that match the given conditions in the given
%% repository name.
-spec find_by(
  atom(), sumo_schema_name(), proplists:proplist(),
  pos_integer(), pos_integer()
) -> [#sumo_doc{}].
find_by(Name, DocName, Conditions, Limit, Offset) ->
  gen_server:call(Name, {find_by, DocName, Conditions, Limit, Offset}).

%% @doc Finds documents that match the given conditions in the given
%% repository name.
-spec find_by(
  atom(), sumo_schema_name(), proplists:proplist()
) -> [#sumo_doc{}].
find_by(Name, DocName, Conditions) ->
  gen_server:call(Name, {find_by, DocName, Conditions}).

%% @doc Calls a custom function in the given repository name.
-spec call(atom(), sumo_schema_name(), atom(), [term()]) -> term().
call(Name, DocName, Function, Args) ->
  gen_server:call(Name, {call, DocName, Function, Args}).

%% @doc Creates the schema of the given docs in the given repository name.
-spec create_schema(atom(), #sumo_schema{}) -> ok.
create_schema(Name, #sumo_schema{}=Schema) ->
  gen_server:call(Name, {create_schema, Schema}).

%% @doc Starts and links a new process for the given repo implementation.
-spec start_link(atom(), module(), [term()]) -> {ok, pid()}.
start_link(Name, Module, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, [Module,Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server stuff.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Called by start_link.
-spec init([term()]) -> {ok, #state{}}.
init([Module, Options]) ->
  {ok, HState} = Module:init(Options),
  {ok, #state{handler=Module, handler_state=HState}}.

%% @doc handles calls.
-spec handle_call(term(), pid(), #state{}) -> term().
handle_call(
  {persist, #sumo_doc{name=DocName}=Doc}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  {ok, DocState, NewState} = Handler:persist(Doc, HState),
  {reply, DocState, State#state{handler_state=NewState}};

handle_call(
  {delete, DocName, Id}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  {ok, Success, NewState} = Handler:delete(DocName, Id, HState),
  {reply, Success, State#state{handler_state=NewState}};

handle_call(
  {delete_all, DocName}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  {ok, NewState} = Handler:delete_all(DocName, HState),
  {reply, ok, State#state{handler_state=NewState}};

handle_call(
  {find_by, DocName, Conditions}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  {ok, Docs, NewState} = Handler:find_by(DocName, Conditions, HState),
  {reply, Docs, State#state{handler_state=NewState}};

handle_call(
  {find_by, DocName, Conditions, Limit, Offset}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  {ok, Docs, NewState} = Handler:find_by(
    DocName, Conditions, Limit, Offset, HState
  ),
  {reply, Docs, State#state{handler_state=NewState}};

handle_call(
  {call, DocName, Function, Args}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  RealArgs = lists:append(Args, [DocName, HState]),
  {ok, Result, NewState} = erlang:apply(Handler, Function, RealArgs),
  {reply, Result, State#state{handler_state=NewState}};

handle_call(
  {create_schema, #sumo_schema{name=Name}=Schema}, _From,
  #state{handler=Handler,handler_state=HState}=State
) ->
  lager:info("Creating schema for: ~p", [Name]),
  {ok, NewState} = Handler:create_schema(Schema, HState),
  {reply, ok, State#state{handler_state=NewState}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
