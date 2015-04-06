%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author Juan Facorro <juan.facorro@inakanetworks.com>
%%% @doc Sumo DB Store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(sumo_test_store).

-behaviour(sumo_store).

-include_lib("emysql/include/emysql.hrl").

%%% Default store functions.
-export(
   [ init/1
   , create_schema/2
   , delete_all/2
   , delete_by/3
   , execute/2
   , execute/3
   , find_all/2
   , find_all/5
   , find_by/3
   , find_by/5
   , find_by/6
   , persist/2
   ]).

%%% Custom store functions.
-export([
         takes_too_long/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_store default functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% should be sumo_store_mysql:state(), we miss the -extends directive
-type state() :: term().

-spec init(term()) -> {ok, state()}.
-spec persist(sumo_internal:doc(), state()) ->
                 sumo_store:result(sumo_internal:doc(), state()).
-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
                   sumo_store:result(sumo_store:affected_rows(), state()).
-spec delete_all(sumo:schema_name(), state()) ->
                    sumo_store:result(sumo_store:affected_rows(), state()).
-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
                 sumo_store:result([sumo_internal:doc()], state()).
-spec find_by(sumo:schema_name(), sumo:conditions(), non_neg_integer(),
              non_neg_integer(), state()) ->
                 sumo_store:result([sumo_internal:doc()], state()).
-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).

init(Options) ->
  sumo_store_mysql:init(Options).

create_schema(Schema, State) ->
  sumo_store_mysql:create_schema(Schema, State).

delete_all(DocName, State) ->
  sumo_store_mysql:delete_all(DocName, State).

delete_by(DocName, Conditions, State) ->
  sumo_store_mysql:delete_by(DocName, Conditions, State).

execute(Query, State) ->
  sumo_store_mysql:execute(Query, State).

execute(Query, Args, State) ->
  sumo_store_mysql:execute(Query, Args, State).

find_all(DocName, State) ->
  sumo_store_mysql:find_all(DocName, State).

find_all(DocName, OrderField, Limit, Offset, State) ->
  sumo_store_mysql:find_all(DocName, OrderField, Limit, Offset, State).

find_by(DocName, Conditions, State) ->
  sumo_store_mysql:find_by(DocName, Conditions, State).

find_by(DocName, Conditions, Limit, Offset, State) ->
  sumo_store_mysql:find_by(DocName, Conditions, Limit, Offset, State).

find_by(DocName, Conditions, SortFields, Limit, Offset, State) ->
  sumo_store_mysql:find_by(
    DocName, Conditions, SortFields, Limit, Offset, State
  ).

persist(Doc, State) ->
  sumo_store_mysql:persist(Doc, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_store custom functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec takes_too_long(atom(), term()) ->
                        [thoughtz_thoughtz:thought()].
takes_too_long(_DocName, State) ->
  timer:sleep(1000),
  {ok, {docs, []}, State}.
