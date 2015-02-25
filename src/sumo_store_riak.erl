%%% @hidden
%%% @doc Riak store implementation.
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
-module(sumo_store_riak).
-author("Carlos Andres Bolanos <candres.bolanos@inakanetworks.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behavior(sumo_store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public API.
-export([init/1]).
-export([create_schema/2]).
-export([persist/2]).
-export([delete_by/3, delete_all/2]).
-export([find_all/2, find_all/5, find_by/3, find_by/5, find_by/6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {conn :: pid(), encoding :: atom(), bucket :: binary()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, term()}.
init(Options) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Options),
  State = sumo_backend_riak:get_state(Backend),
  {ok, State}.

-spec persist(sumo_internal:doc(), state()) ->
  sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, #state{conn = Conn, encoding = Enc, bucket = Bucket} = State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = case sumo_internal:get_field(IdField, Doc) of
         undefined -> next_id(32);
         Id0       -> Id0
       end,
  NewDoc = sumo_internal:set_field(IdField, Id, Doc),
  Fields = sumo_internal:doc_fields(NewDoc),
  EncObj = enc(Fields, Enc),
  %% Create Riak Object
  Object = riakc_obj:new(Bucket, iolist_to_binary(Id), EncObj, ctype(Enc)),
  %% Store the object
  case riakc_pb_socket:put(Conn, Object) of
    {error, Error} ->
      {error, Error, State};
    _ ->
      {ok, NewDoc, State}
  end.

-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, #state{conn = Conn, bucket = Bucket} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case riakc_pb_socket:delete(Conn, Bucket, iolist_to_binary(Key)) of
        ok ->
          {ok, 1, State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      %% @todo query keys that match with conditions and then delete them.
      {ok, 1, State}
  end.

-spec delete_all(sumo:schema_name(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(_DocName, #state{conn = Conn, bucket = Bucket} = State) ->
  %% @todo Optimization required -- this should be a batch op.
  %% Asking Riak to generate a list of all keys in a production environment
  %% is generally a bad idea. It's an expensive operation.
  Delete = fun(K, Acc) ->
             case riakc_pb_socket:delete(Conn, Bucket, K) of
               ok         -> Acc + 1;
               {error, _} -> Acc
             end
           end,
  Keys = case riakc_pb_socket:list_keys(Conn, Bucket) of
           {ok, LK} -> LK;
           _        -> []
         end,
  Count = lists:foldl(Delete, 0, Keys),
  {ok, Count, State}.

-spec find_all(sumo:schema_name(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName,
         #state{conn = Conn, bucket = Bucket, encoding = Enc} = State) ->
  %% @todo Optimization required -- this should be a batch op.
  %% Asking Riak to generate a list of all keys in a production environment
  %% is generally a bad idea. It's an expensive operation.
  case riakc_pb_socket:list_keys(Conn, Bucket) of
    {ok, Keys} ->
      Fun = fun(Item) -> map_to_doc(DocName, Item) end,
      Docs = lists:map(Fun, fetch_bulk(Conn, Bucket, Enc, Keys)),
      {ok, Docs, State};
    _ ->
      {ok, [], State}
  end.

-spec find_all(
  sumo:schema_name(),
  term(),
  non_neg_integer(),
  non_neg_integer(),
  state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, _SortFields, Limit, Offset, State) ->
  find_by(DocName, [], Limit, Offset, State).

-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

-spec find_by(
  sumo:schema_name(),
  sumo:conditions(),
  non_neg_integer(),
  non_neg_integer(),
  state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName,
        Conditions,
        _Limit,
        _Offset,
        #state{conn = Conn, bucket = Bucket, encoding = Enc} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case riakc_pb_socket:get(Conn, Bucket, iolist_to_binary(Key)) of
        {ok, RiakObj} ->
          Val = map_to_doc(DocName, dec(riakc_obj:get_value(RiakObj), Enc)),
          {ok, [Val], State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case riakc_pb_socket:mapred_bucket(Conn, Bucket, Query) of
        {ok, [{_, Results}]} ->
          F = fun(Val, Acc) -> [map_to_doc(DocName, dec(Val, Enc)) | Acc] end,
          NewRes = lists:foldl(F, [], Results),
          {ok, NewRes, State};
        {error, Error} ->
          {error, Error, State}
      end
  end.

%% XXX We should have a DSL here, to allow querying in a known language
%% to be translated by each driver into its own.
-spec find_by(
  sumo:schema_name(),
  sumo:conditions(),
  term(),
  non_neg_integer(),
  non_neg_integer(),
  state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_by(_DocName, _Conditions, _SortFields, _Limit, _Offset, State) ->
  {error, not_supported, State}.

%% XXX: Refactor:
%% Requires {length, X} to be the first field attribute in order to form the
%% correct query. :P
%% If no indexes are defined, will put an extra comma :P
%% Maybe it would be better to just use ALTER statements instead of trying to
%% create the schema on the 1st pass. Also, ALTER statements might be better
%% for when we have migrations.
-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(_Schema, State) ->
  %% @todo Search Schema (Solr), and probably create 2i into the given schema
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ctype(bert) -> "application/x-erlang-binary";
ctype(_)    -> "application/json".

enc(Data, Encoding) ->
  case Encoding of
    bert ->
      term_to_binary(Data, [compressed]);
    _ ->
      jiffy:encode(Data, [uescape])
  end.

dec(Data, Encoding) ->
  case Encoding of
    bert ->
      binary_to_term(Data);
    _ ->
      jiffy:decode(Data, [return_maps])
  end.

map_to_doc(DocName, Item) ->
  Fun = fun (Key, Doc) ->
          FieldName = binary_to_atom(Key, utf8),
          Value = maps:get(Key, Item),
          sumo_internal:set_field(FieldName, Value, Doc)
        end,
  Keys = maps:keys(Item),
  lists:foldl(Fun, sumo_internal:new_doc(DocName), Keys).

next_id(Len) ->
  <<A1:32, A2:32, A3:32>> = crypto:strong_rand_bytes(12),
  random:seed({A1, A2, A3}),
  Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).

fetch_bulk(Conn, Bucket, Enc, Keys) ->
  F = fun(K, Acc) ->
        case riakc_pb_socket:get(Conn, Bucket, K) of
          {ok, RiakObj} ->
            Val = riakc_obj:get_value(RiakObj),
            [dec(Val, Enc) | Acc];
          {error, _} ->
            Acc
        end
      end,
  lists:foldl(F, [], Keys).

norm_conditions(Conditions) ->
  F = fun({X, Y}, Acc) when is_list(Y) ->
        [{atom_to_binary(X, utf8), iolist_to_binary(Y)} | Acc];
      ({X, Y}, Acc) ->
        [{atom_to_binary(X, utf8), Y} | Acc]
      end,
  lists:foldl(F, [], Conditions).

build_query(Conditions) ->
  [{map, {modfun, sumo_store_riak_mapred, map_object_value_by_index},
    norm_conditions(Conditions), true}].
