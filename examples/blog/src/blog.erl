%%% @doc Main module for the blog example.
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
-module(blog).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Posts API.
-export([
  total_posts/0, new_post/3, save_post/1, del_post/0, del_post/1, find_post/1
]).

%%% Author API.
-export([
  new_author/2, save_author/1, del_author/0, del_author/1, del_author_by_name/1,
  find_author/1, find_all_authors/2, find_authors_by_name/3
]).

%%% Reader API.
-export([new_reader/2, save_reader/1, del_reader/0, find_reader/1]).

%%% Vote API.
-export([new_vote/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Code starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Paginates all authors, sorts by name.
-spec find_all_authors(pos_integer(), pos_integer()) -> [blog_author:author()].
find_all_authors(Limit, Offset) ->
  sumo:find_all(blog_author, name, Limit, Offset).

-spec find_authors_by_name(string(), pos_integer(), pos_integer()) -> [blog_author:author()].
find_authors_by_name(Name, Limit, Offset) ->
  sumo:find_by(blog_author, [{name, list_to_atom(Name)}], Limit, Offset).

%% @doc Finds a post given the id.
-spec find_post(blog_post:id()) -> blog_post:post()|notfound.
find_post(Id) ->
  sumo:find(blog_post, Id).

%% @doc Finds an author, given the id.
-spec find_author(blog_author:id()) -> blog_author:author()|notfound.
find_author(Id) ->
  sumo:find(blog_author, Id).

%% @doc Find a reader, given the id.
-spec find_reader(blog_reader:id()) -> blog_reader:reader()|notfound.
find_reader(Id) ->
  sumo:find(blog_reader, Id).

%% @doc Returns all available posts.
-spec total_posts() -> pos_integer().
total_posts() ->
  sumo:call(blog_post, total_posts).

%% @doc Deletes all authors.
-spec del_author() -> ok.
del_author() ->
  sumo:delete_all(blog_author).

%% @doc Deletes all posts.
-spec del_post() -> ok.
del_post() ->
  sumo:delete_all(blog_post).

%% @doc Deletes all readers.
-spec del_reader() -> ok.
del_reader() ->
  sumo:delete_all(blog_reader).

%% @doc Deletes the given author.
-spec del_author_by_name(string()) -> pos_integer().
del_author_by_name(Name) ->
  sumo:delete_by(blog_author, [{name, Name}]).

%% @doc Deletes the given author.
-spec del_author(blog_author:author()) -> ok.
del_author(Author) ->
  sumo:delete(blog_author, blog_author:id(Author)).

%% @doc Deletes the given post.
-spec del_post(blog_post:post()) -> ok.
del_post(Post) ->
  sumo:delete(blog_post, blog_post:id(Post)).

%% @doc Updates an author.
-spec save_author(blog_author:author()) -> ok.
save_author(Author) ->
  sumo:persist(blog_author, Author).

%% @doc Updates a post.
-spec save_post(blog_post:post()) -> ok.
save_post(Post) ->
  sumo:persist(blog_post, Post).

%% @doc Updates a reader.
-spec save_reader(blog_reader:reader()) -> ok.
save_reader(Reader) ->
  sumo:persist(blog_reader, Reader).

%% @doc Creates a new author.
-spec new_author(string(), binary()) -> blog_author:author().
new_author(Name, Photo) ->
  sumo:persist(blog_author, blog_author:new(Name, Photo)).

%% @doc Creates a new post.
-spec new_post(string(), string(), string()) -> blog_post:post().
new_post(Title, Content, Author) ->
  sumo:persist(
    blog_post, blog_post:new(Title, Content, blog_author:id(Author))
  ).

%% @doc Creates a new blog reader.
-spec new_reader(string(), string()) -> blog_reader:reader().
new_reader(Name, Email) ->
  sumo:persist(blog_reader, blog_reader:new(Name, Email)).

%% @doc Creates a new vote.
-spec new_vote(blog_reader:id(), blog_post:id()) -> blog_vote:vote().
new_vote(ReaderId, PostId) ->
  sumo:persist(blog_vote, blog_vote:new(ReaderId, PostId)).

