%%% @doc A blog reader (user).
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
-module(blog_reader).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behaviour(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/2]).
-export([id/1, email/1, name/1]).
-export([update_email/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type reader() :: proplists:proplist().
-type id() :: pos_integer().
-export_type([reader/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a new reader.
-spec new(string(), string()) -> reader().
new(Name, Email) when is_list(Name), is_list(Email) ->
  create(undefined, Name, Email).

%% @doc Returns a new reader (internal).
-spec create(undefined|id(), string(), string()) -> reader().
create(Id, Name, Email) when is_list(Name), is_list(Email) ->
  [{id, Id}, {name, Name}, {email, Email}].

%% @doc Returns the id of the given reader.
-spec id(reader()) -> id().
id(Reader) when is_list(Reader) ->
  get(id, Reader).

%% @doc Returns the email of the given reader.
-spec email(reader()) -> string().
email(Reader) when is_list(Reader) ->
  get(email, Reader).

%% @doc Returns the name of the given reader.
-spec name(reader()) -> string().
name(Reader) when is_list(Reader) ->
  get(name, Reader).

%% @doc Updated the email for the given reader.
-spec update_email(string(), reader()) -> reader().
update_email(Email, Reader) when is_list(Email) ->
  set(email, Email, Reader).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Generically returns an attibute of the given reader.
-spec get(atom(), reader()) -> id() | string().
get(Key, Reader) when is_atom(Key), is_list(Reader) ->
  proplists:get_value(Key, Reader).

%% @doc Generically set an attribute of the given reader.
-spec set(atom(), term(), reader()) -> reader().
set(Key, Value, Reader) when is_atom(Key), is_list(Reader) ->
  lists:keyreplace(Key, 1, Reader, {Key, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:model()) -> reader().
sumo_wakeup(Data) ->
  maps:to_list(Data).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(reader()) -> sumo:model().
sumo_sleep(Reader) ->
  maps:from_list(Reader).

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(reader, [
    sumo:new_field(id, integer, [not_null, auto_increment, id]),
    sumo:new_field(name, string, [{length, 128}, not_null, unique]),
    sumo:new_field(email, string, [{length, 128}, index])
  ]).
