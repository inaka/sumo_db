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
%% Types and Macros.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {conn :: pid(), bucket :: binary(), index :: binary()}).
-type state() :: #state{}.

-define(KEY_FIND(Key_, TupleList_),
  case lists:keyfind(Key_, 1, TupleList_) of
    {_, V_} -> V_;
    _       -> undefined
  end
).

-define(NORM_DOC_FIELD(Src_),
  re:replace(
    Src_, <<"_register|_set|_counter|_flag|_map">>, <<"">>,
    [{return, binary}, global])
).

-define(REG(Val_),
  fun(R_) -> riakc_register:set(Val_, R_) end
).

-define(RMAP_FETCH(Key_, Map_),
  case riakc_map:find({Key_, register}, Map_) of
    {ok, Val_} -> Val_;
    _           -> undefined
  end
).

-define(RMAP_UPDATE(KV_, Map_),
  {Key_, Val_} = KV_,
  riakc_map:update({Key_, register}, ?REG(Val_), Map_)
).

-define(FETCH_MAP(Conn_, Bucket_, Key_),
  riakc_pb_socket:fetch_type(Conn_,Bucket_, Key_)
).

-define(UPDATE_MAP(Conn_, Bucket_, Key_, Map_),
  riakc_pb_socket:update_type(Conn_, Bucket_, Key_, riakc_map:to_op(Map_))
).

-define(SEARCH(Conn_, Index_, Query_),
  riakc_pb_socket:search(Conn_, Index_, Query_)
).

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

-spec persist(
  sumo_internal:doc(), state()
) -> sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, #state{conn = Conn, bucket = Bucket} = State) ->
  NewDoc = new_doc(Doc),
  DocName = sumo_internal:doc_name(NewDoc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = iolist_to_binary(sumo_internal:get_field(IdField, NewDoc)),
  case ?UPDATE_MAP(Conn, Bucket, Id, doc_to_rmap(NewDoc)) of
    {error, Error} ->
      {error, Error, State};
    _ ->
      {ok, NewDoc, State}
  end.

-spec delete_by(
  sumo:schema_name(), sumo:conditions(), state()
) -> sumo_store:result(sumo_store:affected_rows(), state()).
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

-spec delete_all(
  sumo:schema_name(), state()
) -> sumo_store:result(sumo_store:affected_rows(), state()).
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

-spec find_all(
  sumo:schema_name(), state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, #state{conn = Conn, bucket = Bucket} = State) ->
  %% @todo Optimization required -- this should be a batch op.
  %% Asking Riak to generate a list of all keys in a production environment
  %% is generally a bad idea. It's an expensive operation.
  case riakc_pb_socket:list_keys(Conn, Bucket) of
    {ok, Keys} ->
      F = fun(Item, Acc) -> [rmap_to_doc(DocName, Item) | Acc] end,
      Docs = lists:foldl(F, [], fetch_bulk(Conn, Bucket, Keys)),
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
        #state{conn = Conn, bucket = Bucket, index = Index} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case ?FETCH_MAP(Conn, Bucket, iolist_to_binary(Key)) of
        {ok, RMap} ->
          Val = rmap_to_doc(DocName, RMap),
          {ok, [Val], State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case ?SEARCH(Conn, Index, Query) of
        {ok, {search_results, Results, _, _Count}} ->
          F = fun({_, KV}, Acc) -> [kv_to_doc(DocName, KV) | Acc] end,
          NewRes = lists:foldl(F, [], Results),
          {ok, NewRes, State};
        {error, Error} ->
          {error, Error, State}
      end
  end.

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

-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(_Schema, State) ->
  %% @todo Search Schema (Solr), and probably create 2i into the given schema
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
new_doc(Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = case sumo_internal:get_field(IdField, Doc) of
         undefined -> next_id(32);
         Id0       -> Id0
       end,
  sumo_internal:set_field(IdField, Id, Doc).

%% @private
doc_to_rmap(Doc) ->
  Fields = sumo_internal:doc_fields(Doc),
  F = fun({K, V}, Acc) ->
        ?RMAP_UPDATE({atom_to_binary(K, utf8), to_bin(V)}, Acc)
      end,
  lists:foldl(F, riakc_map:new(), maps:to_list(Fields)).

%% @private
rmap_to_doc(DocName, RMap) ->
  F = fun({{K, _}, V}, Acc) ->
        sumo_internal:set_field(binary_to_atom(K, utf8), V, Acc)
      end,
  lists:foldl(F, sumo_internal:new_doc(DocName), riakc_map:value(RMap)).

%% @private
kv_to_doc(DocName, KV) ->
  F = fun({K, V}, Acc) ->
        NK = ?NORM_DOC_FIELD(K),
        sumo_internal:set_field(binary_to_atom(NK, utf8), V, Acc)
      end,
  lists:foldl(F, sumo_internal:new_doc(DocName), KV).

%% @private
fetch_bulk(Conn, Bucket, Keys) ->
  F = fun(K, Acc) ->
        case ?FETCH_MAP(Conn, Bucket, K) of
          {ok, RMap} -> [RMap | Acc];
          _          -> Acc
        end
      end,
  lists:foldl(F, [], Keys).

%% @private
build_query({q_str, Q}) when is_binary(Q) ->
  Q;
build_query(PL) when is_list(PL) ->
  build_query1(PL, <<"">>);
build_query(_) ->
  <<"*:*">>.

build_query1([], Acc) ->
  Acc;
build_query1([{_, [{_, _} | _T0]} = KV | T], <<"">>) ->
  build_query1(T, <<(<<"(">>)/binary, (build_query2(KV))/binary, (<<")">>)/binary>>);
build_query1([{_, [{_, _} | _T0]} = KV | T], Acc) ->
  build_query1(T, <<Acc/binary, (<<" AND (">>)/binary, (build_query2(KV))/binary, (<<")">>)/binary>>);
build_query1([{K, V} | T], <<"">>) ->
  build_query1(T, <<(query_eq(K, V))/binary>>);
build_query1([{K, V} | T], Acc) ->
  build_query1(T, <<Acc/binary, (<<" AND ">>)/binary, (query_eq(K, V))/binary>>).

query_eq(K, V) ->
  <<(atom_to_binary(K, utf8))/binary,
    (<<"_register:">>)/binary,
    (to_bin(V))/binary>>.

build_query2({K, [{_, _} | _T] = V}) ->
  F = fun({K_, V_}, Acc) ->
        Eq = <<(atom_to_binary(K_, utf8))/binary,
               (<<"_register:">>)/binary,
               (to_bin(V_))/binary>>,
        case Acc of
          <<"">> ->
            Eq;
          _ ->
            <<Acc/binary, (<<" ">>)/binary, (to_bin(K))/binary,
              (<<" ">>)/binary, Eq/binary>>
        end
      end,
  lists:foldl(F, <<"">>, V).

%% @private
next_id(Len) ->
  <<A1:32, A2:32, A3:32>> = crypto:strong_rand_bytes(12),
  random:seed({A1, A2, A3}),
  Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).

%% @private
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) ->
  Data.
