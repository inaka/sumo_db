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

-include_lib("include/epers_doc.hrl").

-behavior(epers_doc).

-export([epers_schema/0, epers_wakeup/1]).
-export([new/3]).
-export([id/1, author/1, title/1, content/1, update_content/2, update_title/2]).

-type post() :: [attr()].
-type attr() :: {key(), term()}.
-type key() :: id|title|content|author_id.
-type id() :: pos_integer().

-export_type([post/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new(Title, Content, AuthorId) when is_list(Title), is_list(Content) ->
  create(undefined, Title, Content, AuthorId).

create(Id, Title, Content, AuthorId)
  when is_list(Title), is_list(Content), is_integer(AuthorId) ->
  [{id, Id}, {title, Title}, {content, Content}, {author_id, AuthorId}].

id(State) when is_list(State) ->
  get(id, State).

author(State) when is_list(State) ->
  get(author, State).

title(State) when is_list(State) ->
  get(title, State).

content(State) when is_list(State) ->
  get(content, State).

update_title(Title, State) when is_list(Title), is_list(State) ->
  set(title, Title, State).

update_content(Content, State) when is_list(Content), is_list(State) ->
  set(content, Content, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Key, State) when is_atom(Key), is_list(State) ->
  proplists:get_value(Key, State).

set(Key, Value, State) when is_atom(Key), is_list(State) ->
  lists:keyreplace(Key, 1, State, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% eper behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epers_wakeup(#epers_doc{}=Doc) ->
  [
    {id, epers:get_field(id, Doc)},
    {title, epers:get_field(title, Doc)},
    {content, epers:get_field(content, Doc)},
    {author_id, epers:get_field(author_id, Doc)}
  ].

epers_schema() ->
  epers:new_schema(?MODULE, [
    epers:new_field(id, integer, [not_null, auto_increment, id]),
    epers:new_field(title, string, [{length, 128}, not_null, unique]),
    epers:new_field(content, text),
    epers:new_field(author_id, integer, [index])
  ]).
