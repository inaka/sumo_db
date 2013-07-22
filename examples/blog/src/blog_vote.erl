%%% @doc Readers can vote posts.
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
-module(blog_vote).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include_lib("include/sumo_doc.hrl").

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/2]).
-export([id/1, post_id/1, reader_id/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type vote() :: proplists:proplist().
-type id() :: pos_integer().
-export_type([vote/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a new vote.
-spec new(blog_reader:id(), blog_post:id()) -> vote().
new(ReaderId, PostId) ->
  create(undefined, ReaderId, PostId).

%% @doc Returns a new vote (internal).
-spec create(id(), blog_reader:id(), blog_post:id()) -> vote().
create(Id, ReaderId, PostId) ->
  [{id, Id}, {reader_id, ReaderId}, {post_id, PostId}].

%% @doc Returns the id of the given vote.
-spec id(vote()) -> id().
id(Vote) when is_list(Vote) ->
  get(id, Vote).

%% @doc Returns the post id of the given vote.
-spec post_id(vote()) -> blog_post:id().
post_id(Vote) when is_list(Vote) ->
  get(post_id, Vote).

%% @doc Returns the reader id of the given post.
-spec reader_id(vote()) -> blog_reader:id().
reader_id(Vote) when is_list(Vote) ->
  get(reader_id, Vote).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generically returns an attibute of the given vote.
-spec get(atom(), vote()) -> term().
get(Key, Vote) when is_atom(Key), is_list(Vote) ->
  proplists:get_value(Key, Vote).

%% @doc Generically set an attribute of the given vote.
%-spec set(atom(), term(), vote()) -> proplists:proplist().
%set(Key, Value, Vote) when is_atom(Key), is_list(Vote) ->
%  lists:keyreplace(Key, 1, Vote, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(proplists:proplist()) -> vote().
sumo_wakeup(Data) ->
  Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(vote()) -> proplists:proplist().
sumo_sleep(Vote) ->
  Vote.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> #sumo_schema{}.
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id, integer, [not_null, auto_increment, id]),
    sumo:new_field(post_id, integer),
    sumo:new_field(reader_id, integer)
  ]).
