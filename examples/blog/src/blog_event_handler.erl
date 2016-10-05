%%% @doc The blog post event handler.
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
-module(blog_event_handler).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behaviour(gen_event).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
  init/1, terminate/2, handle_info/2,
  handle_call/2, code_change/3, handle_event/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
  {ok, []}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event({post, schema_created, []}, State) ->
  lager:info("The blog post schema has been created"),
  {ok, State};

handle_event({post, deleted, [Id]}, State) ->
  lager:info("The blog post ~p has been deleted", [Id]),
  {ok, State};

handle_event({post, deleted_all, []}, State) ->
  lager:info("All blog posts entries have been deleted"),
  {ok, State};

handle_event({post, created, [Entity]}, State) ->
  Id = proplists:get_value(id, Entity),
  lager:info("Blog post ~p created", [Id]),
  {ok, State};

handle_event({post, updated, [Entity]}, State) ->
  Id = proplists:get_value(id, Entity),
  lager:info("Blog post ~p updated", [Id]),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.
