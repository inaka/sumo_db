%%% @doc A blog post.
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
-module(blog_post).
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
-export([new/3]).
-export([id/1, author/1, title/1, content/1, update_content/2, update_title/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type post() :: proplists:proplist().
-type id() :: pos_integer().
-export_type([post/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a new post.
-spec new(string(), string(), blog_author:id()) -> post().
new(Title, Content, AuthorId) when is_list(Title), is_list(Content) ->
  create(undefined, Title, Content, AuthorId).

%% @doc Returns a new post (internal).
-spec create(id(), string(), string(), blog_author:id()) -> post().
create(Id, Title, Content, AuthorId)
  when is_list(Title), is_list(Content), is_integer(AuthorId) ->
  [{id, Id}, {title, Title}, {content, Content}, {author_id, AuthorId}].

%% @doc Returns the id of the given post.
-spec id(post()) -> id().
id(Post) when is_list(Post) ->
  get(id, Post).

%% @doc Returns the author of the given post.
-spec author(post()) -> blog_author:id().
author(Post) when is_list(Post) ->
  get(author, Post).

%% @doc Returns the title of the given post.
-spec title(post()) -> string().
title(Post) when is_list(Post) ->
  get(title, Post).

%% @doc Returns the content of the given post.
-spec content(post()) -> string().
content(Post) when is_list(Post) ->
  get(content, Post).

%% @doc Updated the title of the given post.
-spec update_title(string(), post()) -> post().
update_title(Title, Post) when is_list(Title), is_list(Post) ->
  set(title, Title, Post).

%% @doc Updated the content of the given post.
-spec update_content(string(), post()) -> post().
update_content(Content, Post) when is_list(Content), is_list(Post) ->
  set(content, Content, Post).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generically returns an attibute of the given post.
-spec get(atom(), post()) -> term().
get(Key, Post) when is_atom(Key), is_list(Post) ->
  proplists:get_value(Key, Post).

%% @doc Generically set an attribute of the given post.
-spec set(atom(), term(), post()) -> post().
set(Key, Value, Post) when is_atom(Key), is_list(Post) ->
  lists:keyreplace(Key, 1, Post, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(proplists:proplist()) -> post().
sumo_wakeup(Data) ->
  Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(post()) -> proplists:proplist().
sumo_sleep(Post) ->
  Post.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> #sumo_schema{}.
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id, integer, [not_null, auto_increment, id]),
    sumo:new_field(title, string, [{length, 128}, not_null, unique]),
    sumo:new_field(content, text),
    sumo:new_field(author_id, integer, [index])
  ]).

%% We don't have the extends module attribute in R16, so this was moved out from
%% the old blog_post_repo in the example, we should allocate some time to create
%% a proper parse transform for this.
% -spec total_posts(sumo_schema_name(), term() ) -> {ok, {raw, pos_integer}, term()} | {ok, error, term()}.
% count(DocName, State) ->
%   Sql = "SELECT COUNT(1) FROM `" ++ atom_to_list(DocName) ++ "`",
%   Result = sumo_repo_mysql:execute(Sql, State),
%   case Result of
%     #result_packet{rows=[[N]]} -> {ok, {raw, N}, State};
%     _ -> {ok, error, State}
%   end.