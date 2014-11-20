%%% @hidden
%%% @doc PostgreSql repository implementation.
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
-module(sumo_store_pgsql).
-author("Juan Facorro <juan@inaka.net>").
-license("Apache License 2.0").

-behavior(sumo_store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public API.
-export([init/1]).
-export([create_schema/2]).
-export([persist/2]).
-export([delete/3, delete_by/3, delete_all/2]).
-export([find_all/2, find_all/5, find_by/3, find_by/5, find_by/6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{conn => term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, term()}.
init(Options) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Options),
  Conn    = sumo_backend_pgsql:get_connection(Backend),
  {ok, #{conn => Conn}}.

-spec persist(sumo_internal:doc(), state()) ->
  sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, State) ->
  lager:info("persist"),
  {ok, Doc, State}.

-spec delete(sumo:schema_name(), sumo:field_value(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete(_DocName, _Id, State) ->
  lager:info("deleting..."),
  {ok, true, State}.

-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(_DocName, _Conditions, State) ->
  lager:info("deleting by..."),
  {ok, 1, State}.

-spec delete_all(sumo:schema_name(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(_DocName, State) ->
  lager:info("deleting by..."),
  {ok, 1, State}.

-spec find_all(sumo:schema_name(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

-spec find_all(sumo:schema_name(),
               term(),
               non_neg_integer(),
               non_neg_integer(),
               state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, SortFields, Limit, Offset, State) ->
  find_by(DocName, [], SortFields, Limit, Offset, State).

-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, [], 0, 0, State).

-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

%% XXX We should have a DSL here, to allow querying in a known language
%% to be translated by each driver into its own.
-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              term(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(_DocName, _Conditions, _SortFields, _Limit, _Offset, State) ->
  lager:info("finding by..."),
  {ok, [], State}.

%% XXX: Refactor:
%% Requires {length, X} to be the first field attribute in order to form the
%% correct query. :P
%% If no indexes are defined, will put an extra comma :P
%% Maybe it would be better to just use ALTER statements instead of trying to
%% create the schema on the 1st pass. Also, ALTER statements might be better
%% for when we have migrations.
-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(Schema, #{conn := Conn} = State) ->
  lager:info("creating schema..."),
  Name = sumo_internal:schema_name(Schema),
  Fields = sumo_internal:schema_fields(Schema),
  FieldsDql = lists:map(fun create_column/1, Fields),

  Indexes = lists:filter(
    fun(T) -> length(T) > 0 end,
    lists:map(fun create_index/1, Fields)
  ),
  Dql = [
    "CREATE TABLE IF NOT EXISTS ", escape(atom_to_list(Name)), " (",
    string:join(FieldsDql, ","), ",", string:join(Indexes, ","),
    ") "
  ],
  BinDql = iolist_to_binary(Dql),
  StrDql = binary_to_list(BinDql),
  lager:info("~p~n", [StrDql]),

  case pgsql:squery(Conn, StrDql) of
    {error, Error} -> {error, Error, State};
    {ok, [], []} -> {ok, State}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escape(Name) when is_atom(Name) ->
  ["\"", atom_to_list(Name), "\""];
escape(String) ->
  ["\"", String, "\""].

%%% Schema related

create_column(Field) ->
  create_column(
    sumo_internal:field_name(Field),
    sumo_internal:field_type(Field),
    sumo_internal:field_attrs(Field)).

create_column(Name, integer, Attrs) ->
  case lists:member(auto_increment, Attrs) of
    true ->
      [escape(atom_to_list(Name)), create_column_options(Attrs)];
    false ->
      [escape(atom_to_list(Name)), " INTEGER ", create_column_options(Attrs)]
  end;
create_column(Name, float, Attrs) ->
  [escape(atom_to_list(Name)), " FLOAT ", create_column_options(Attrs)];
create_column(Name, text, Attrs) ->
  [escape(atom_to_list(Name)), " TEXT ", create_column_options(Attrs)];
create_column(Name, binary, Attrs) ->
  [escape(atom_to_list(Name)), " BYTEA ", create_column_options(Attrs)];
create_column(Name, string, Attrs) ->
  [escape(atom_to_list(Name)), " VARCHAR ", create_column_options(Attrs)];
create_column(Name, date, Attrs) ->
  [escape(atom_to_list(Name)), " DATE ", create_column_options(Attrs)];
create_column(Name, datetime, Attrs) ->
  [escape(atom_to_list(Name)), " TIMESTAMP ", create_column_options(Attrs)].

create_column_options(Attrs) ->
  lists:filter(fun(T) -> is_list(T) end, lists:map(
    fun(Option) ->
      create_column_option(Option)
    end,
    Attrs
  )).

create_column_option(auto_increment) ->
  [" SERIAL "];
create_column_option(not_null) ->
  [" NOT NULL "];
create_column_option({length, X}) ->
  ["(", integer_to_list(X), ") "];
create_column_option(_Option) ->
  none.

create_index(Field) ->
  Name = sumo_internal:field_name(Field),
  Attrs = sumo_internal:field_attrs(Field),
  lists:filter(fun(T) -> is_list(T) end, lists:map(
    fun(Attr) ->
      create_index(Name, Attr)
    end,
    Attrs
  )).

create_index(Name, id) ->
  ["PRIMARY KEY(", escape(atom_to_list(Name)), ")"];
create_index(Name, unique) ->
  List = atom_to_list(Name),
  ["UNIQUE KEY ", escape(List), " (", escape(List), ")"];
create_index(Name, index) ->
  List = atom_to_list(Name),
  ["KEY ", escape(List), " (", escape(List), ")"];
create_index(_, _) ->
  none.
