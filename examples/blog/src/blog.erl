%%% @doc Main module for the blog example.
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
-module(blog).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%% Posts API.
-export([
  total_posts/0, new_post/3, save_post/1, del_post/0, del_post/1, find_post/1
]).

%%% Author API.
-export([
  new_author/2, save_author/1, del_author/0, del_author/1, find_author/1
]).

%%% Reader API.
-export([new_reader/2, save_reader/1, del_reader/0, find_reader/1]).

%%% Vote API.
-export([new_vote/2]).

%% @doc Finds a post given the id.
-spec find_post(blog_post:id()) -> blog_post:post()|notfound.
find_post(Id) ->
  epers:find(blog_post, Id).

%% @doc Finds an author, given the id.
-spec find_author(blog_author:id()) -> blog_author:author()|notfound.
find_author(Id) ->
  epers:find(blog_author, Id).

find_reader(Id) ->
  epers:find(blog_reader, Id).

total_posts() ->
  epers:call(blog_post, total_posts).

del_author() ->
  epers:delete_all(blog_author).

del_post() ->
  epers:delete_all(blog_post).

del_reader() ->
  epers:delete_all(blog_reader).

del_author(Author) ->
  epers:delete(blog_author, blog_author:id(Author)).

del_post(Post) ->
  epers:delete(blog_post, blog_post:id(Post)).

save_author(Author) ->
  epers:persist(blog_author, Author).

save_post(Post) ->
  epers:persist(blog_post, Post).

save_reader(Reader) ->
  epers:persist(blog_reader, Reader).

new_author(Name, Photo) ->
  epers:persist(blog_author, blog_author:new(Name, Photo)).

new_post(Title, Content, Author) ->
  epers:persist(
    blog_post, blog_post:new(Title, Content, blog_author:id(Author))
  ).

new_reader(Name, Email) ->
  epers:persist(blog_reader, blog_reader:new(Name, Email)).

new_vote(ReaderId, PostId) ->
  epers:persist(blog_vote, blog_vote:new(ReaderId, PostId)).
