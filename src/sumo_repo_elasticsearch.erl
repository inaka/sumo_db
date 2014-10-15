%%% @doc MongoDB repository implementation.
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
-module(sumo_repo_elasticsearch).
-author("Juan Facorro <juan@inaka.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behavior(sumo_repo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([
         init/1, create_schema/2, persist/2, find_by/3, find_by/5, find_all/2,
         delete/3, delete_by/3, delete_all/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type state() ::
        #{index => binary(),
          pool_name => atom()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Options) ->
    %% ElasticSearch client uses poolboy to handle its own pool of workers
    %% so no pool is required.
    Backend = proplists:get_value(storage_backend, Options),
    Index = case sumo_backend_elasticsearch:get_index(Backend) of
                Idx when is_list(Idx) -> list_to_binary(Idx);
                Idx when is_binary(Idx) -> Idx;
                _ -> throw(invalid_index)
            end,
    PoolName = sumo_backend_elasticsearch:get_pool_name(Backend),

    {ok, #{index => Index, pool_name => PoolName}}.

-spec persist(sumo:doc(), state()) -> sumo:user_doc().
persist(Doc, #{index := Index, pool_name := PoolName} = State) ->
    DocName = sumo_internal:doc_name(Doc),
    Type = atom_to_binary(DocName, utf8),

    IdField = sumo_internal:id_field_name(DocName),
    Id =  sumo_internal:get_field(IdField, Doc),

    Fields = sumo_internal:doc_fields(Doc),
    FieldsMap = maps:from_list(Fields),

    Doc1 =
        case Id of
            undefined ->
                {ok, Json} =
                    tirerl:insert_doc(PoolName, Index, Type, Id, FieldsMap),

                %% Get the Id that was assigned by elasticsearch.
                GenId = maps:get(<<"_id">>, Json),
                Update = #{doc => maps:from_list([{IdField, GenId}])},

                {ok, _ } =
                    tirerl:update_doc(PoolName, Index, Type, GenId, Update),

                sumo_internal:set_field(IdField, GenId, Doc);
            Id ->
                {ok, _ } =
                    tirerl:update_doc(PoolName, Index, Type, Id, FieldsMap),
                Doc
        end,

    {ok, Doc1, State}.

delete(DocName, Id, State) ->
    delete_by(DocName, [{id, Id}], State).

delete_by(DocName, Conditions, #{index := Index, pool_name := PoolName} = State) ->
    Query = build_query(Conditions),
    Type = atom_to_binary(DocName, utf8),

    {ok, _} = tirerl:delete_by_query(PoolName, Index, Type, Query, []),

    {ok, not_implemented, State}.

delete_all(DocName, #{index := Index, pool_name := PoolName} = State) ->
    lager:debug("dropping type: ~p", [DocName]),
    Type = atom_to_binary(DocName, utf8),
    MatchAll = #{query => #{match_all => #{}}},
    {ok, _} = tirerl:delete_by_query(PoolName, Index, Type, MatchAll, []),
    {ok, unknown, State}.

find_by(DocName, Conditions, Limit, Offset,
        #{index := Index, pool_name := PoolName} = State) ->
    Type = atom_to_binary(DocName, utf8),
    Query = build_query(Conditions, Limit, Offset),

    {ok, #{<<"hits">> := #{<<"hits">> := Results}}} =
        tirerl:search(PoolName, Index, Type, Query),

    Fun = fun(Item) -> map_to_doc(DocName, Item) end,
    Docs = lists:map(Fun, Results),

    {ok, Docs, State}.

find_by(DocName, Conditions, State) ->
    find_by(DocName, Conditions, 0, 0, State).

find_all(DocName, State) ->
    find_by(DocName, [], State).

create_schema(Schema, #{index := Index, pool_name := PoolName} = State) ->
    SchemaName = sumo_internal:schema_name(Schema),
    Fields = sumo_internal:schema_fields(Schema),
    Fun =
        fun
            (Field, Acc) ->
                _Name = sumo_internal:field_name(Field),
                _Attrs = sumo_internal:field_attrs(Field),
                Acc
        end,
    Mappings = lists:foldl(Fun, #{}, Fields),
    lager:debug("creating type: ~p", [SchemaName]),
    {ok, Result} = tirerl:create_index(PoolName, Index, Mappings),
    io:format("~p~n", [Result]),
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_to_doc(DocName, Item) ->
    Values = maps:get(<<"_source">>, Item),
    IdField = sumo_internal:id_field_name(DocName),

    Fun = fun (Key, Doc) ->
              FieldName = binary_to_atom(Key, utf8),
              Value = maps:get(Key, Values),
              sumo_internal:set_field(FieldName, Value, Doc)
          end,
    Keys = maps:keys(Values),
    Doc = lists:foldl(Fun, sumo_internal:new_doc(DocName, []), Keys),
    sumo_internal:set_field(IdField, maps:get(<<"_id">>, Item), Doc).

build_query(Conditions) ->
    build_query(Conditions, 0, 0).

build_query(Conditions, Limit, Offset) ->
        CondFun =
        fun
            ({Key, Value}) when is_list(Value) ->
                #{match => maps:from_list([{Key, list_to_binary(Value)}])};
            (Cond) ->
                #{match => maps:from_list([Cond])}
        end,
    QueryConditions = lists:map(CondFun, Conditions),
    Query = #{query => #{bool => #{must => QueryConditions}}},
    case Limit of
        0 -> Query;
        _ -> Query#{from => Offset,
                    size => Limit}
    end.
