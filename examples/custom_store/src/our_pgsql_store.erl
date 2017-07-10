-module(our_pgsql_store).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("mixer/include/mixer.hrl").

-mixin([sumo_store_pgsql]). % we want to mantain all the sumo_store_pgsql functions and adding ours

-export([count_by_city/3]).

count_by_city(City, DocName, State) ->
  #{conn := Conn} = State,
  Query = [
    "SELECT Count(city) FROM ", escape(DocName),
    " GROUP BY CITY ",
    " HAVING ",
    " city = $1"

  ],
  parse_result(epgsql:equery(Conn, stringify(Query), [City]), State).

%%% Internal

%% @private
escape(Name) when is_atom(Name) ->
  ["\"", atom_to_list(Name), "\""];
escape(String) ->
  ["\"", String, "\""].

%% @private
stringify(Sql) -> binary_to_list(iolist_to_binary(Sql)).

%% @private
parse_result({ok, _, []}, State) ->
  {ok, {raw, 0}, State};
parse_result({ok, _, [{Count}]}, State) ->
  {ok, {raw, Count}, State};
parse_result({error, Error}, State) ->
  {error, Error, State}.
