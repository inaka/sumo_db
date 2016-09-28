%%% @doc A blog post author.
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
-module(blog_author).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behaviour(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/1, new/2]).
-export([id/1, name/1, photo/1, update_photo/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type author() :: proplists:proplist().
-type id() :: pos_integer().
-export_type([author/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a new author.
-spec new(string()) -> author().
new(Name) when is_binary(Name) ->
  create(undefined, Name, <<>>).

%% @doc Returns a new author.
-spec new(string(), binary()) -> author().
new(Name, Photo) when is_binary(Name), is_binary(Photo) ->
  create(undefined, Name, Photo).

%% @doc Returns a new author (internal).
-spec create(undefined|id(), binary(), binary()) -> author().
create(Id, Name, Photo) when is_binary(Name), is_binary(Photo) ->
  [{id, Id}, {name, Name}, {photo, Photo}].

%% @doc Returns the id of the given author.
-spec id(author()) -> id().
id(Author) when is_list(Author) ->
  get(id, Author).

%% @doc Returns the name of the given author.
-spec name(author()) -> string().
name(Author) when is_binary(Author) ->
  get(name, Author).

%% @doc Returns the current author's photo.
-spec photo(author()) -> binary().
photo(Author) when is_list(Author) ->
  get(photo, Author).

%% @doc Updated the photo of the given author.
-spec update_photo(binary(), author()) -> author().
update_photo(Photo, Author) when is_binary(Photo), is_list(Author) ->
  set(photo, Photo, Author).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generically returns an attibute of the given author.
-spec get(atom(), author()) -> term().
get(Key, Author) when is_atom(Key), is_list(Author) ->
  proplists:get_value(Key, Author).

%% @doc Generically set an attribute of the given author.
-spec set(atom(), term(), author()) -> author().
set(Key, Value, Author) when is_atom(Key), is_list(Author) ->
  lists:keyreplace(Key, 1, Author, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:model()) -> author().
sumo_wakeup(Data) ->
  maps:to_list(Data).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(author()) -> sumo:model().
sumo_sleep(Author) ->
  maps:from_list(Author).

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(author, [
    sumo:new_field(id, integer, [not_null, auto_increment, id]),
    sumo:new_field(name, string, [{length, 128}, not_null]),
    sumo:new_field(photo, binary)
  ]).
