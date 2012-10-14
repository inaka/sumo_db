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

-include_lib("include/epers_doc.hrl").

-behavior(epers_doc).

-export([epers_schema/0, epers_wakeup/1]).
-export([new/2]).
-export([id/1, post_id/1, reader_id/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new(ReaderId, PostId) ->
  create(undefined, ReaderId, PostId).

create(Id, ReaderId, PostId) ->
  [{id, Id}, {reader_id, ReaderId}, {post_id, PostId}].

id(State) when is_list(State) ->
  get(id, State).

post_id(State) when is_list(State) ->
  get(post_id, State).

reader_id(State) when is_list(State) ->
  get(reader_id, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Key, State) when is_atom(Key), is_list(State) ->
  proplists:get_value(Key, State).

%set(Key, Value, State) when is_atom(Key), is_list(State) ->
%  lists:keyreplace(Key, 1, State, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% eper behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
epers_wakeup(#epers_doc{}=Doc) ->
  [
    {id, epers:get_field(id, Doc)},
    {post_id, epers:get_field(post_id, Doc)},
    {reader_id, epers:get_field(reader_id, Doc)}
  ].

epers_schema() ->
  epers:new_schema(?MODULE, [
    epers:new_field(id, integer, [not_null, auto_increment, id]),
    epers:new_field(post_id, integer),
    epers:new_field(reader_id, integer)
  ]).
