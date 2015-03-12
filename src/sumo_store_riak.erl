%%% @hidden
%%% @doc Riak store implementation.
%%% <u>Implementation Notes:</u>
%%% <ul>
%%% <li> Riak Data Types as main structures to push/pull data.</li>
%%% <li> Bulk operations (such as: delete_all and find_all) were
%%%      optimized using streaming. Records are streamed in portions
%%%      (using Riak 2i to stream keys first), and then the current
%%%      operation (e.g.: delete the record or accumulate the values
%%%      to return them later) is applied. This allows better memory
%%%      and cpu efficiency.</li>
%%% <li> Query functions were implemented using Riak Search on Data Types,
%%%      to get better performance and flexibility.</li>
%%% </ul>
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

%% Riak quorum parameters.
%% @see <a href="http://docs.basho.com/riak/latest/dev/using/basics"/>
-type r_param() :: r | pr | notfound_ok.
-type w_param() :: w | pw | dw | returnbody.

%% conn: is the Pid of the gen_server that holds the connection with Riak
%% bucket: Riak bucket (per store)
%% index: Riak index to be used by Riak Search
%% read_quorum: Riak read quorum parameters.
%% write_quorum: Riak write quorum parameters.
-record(state, {conn         :: pid(),
                bucket       :: binary(),
                index        :: binary(),
                read_quorum  :: [{r_param(), integer() | (true | false)}],
                write_quorum :: [{w_param(), integer() | (true | false)}]}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(
  term()
) -> {ok, term()}.
init(Opts) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Opts),
  Conn = sumo_backend_riak:get_connection(Backend),
  BucketType = iolist_to_binary(
    proplists:get_value(bucket_type, Opts, <<"maps">>)),
  Bucket = iolist_to_binary(
    proplists:get_value(bucket, Opts, <<"sumo">>)),
  Index = iolist_to_binary(
    proplists:get_value(index, Opts, <<"sumo_index">>)),
  Rq = proplists:get_value(read_quorum, Opts, []),
  Wq = proplists:get_value(write_quorum, Opts, []),
  State = #state{conn = Conn,
                 bucket = {BucketType, Bucket},
                 index = Index,
                 read_quorum = Rq,
                 write_quorum = Wq},
  {ok, State}.

-spec persist(
  sumo_internal:doc(), state()
) -> sumo_store:result(sumo_internal:doc(), state()).
persist(Doc,
        #state{conn = Conn, bucket = Bucket, write_quorum = Wq} = State) ->
  {Id, NewDoc} = new_doc(Doc, State),
  case update_map(Conn, Bucket, Id, doc_to_rmap(NewDoc), Wq) of
    {error, Error} ->
      {error, Error, State};
    _ ->
      {ok, NewDoc, State}
  end.

-spec delete_by(
  sumo:schema_name(), sumo:conditions(), state()
) -> sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName,
          Conditions,
          #state{conn = Conn, bucket = Bucket, index = Index} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case delete_map(Conn, Bucket, iolist_to_binary(Key)) of
        ok ->
          {ok, 1, State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case search_docs_by(DocName, Conn, Index, Query, 0, 0) of
        {ok, {Total, Res}}  ->
          delete_docs(Conn, Bucket, Res),
          {ok, Total, State};
        {error, Error} ->
          {error, Error, State}
      end
  end.

-spec delete_all(
  sumo:schema_name(), state()
) -> sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(_DocName, #state{conn = Conn, bucket = Bucket} = State) ->
  Del = fun({C, B, Kst}, Acc) ->
          lists:foreach(fun(K) -> delete_map(C, B, K) end, Kst),
          Acc + length(Kst)
        end,
  case stream_keys(Conn, Bucket, Del, 0) of
    {ok, Count} -> {ok, Count, State};
    {_, Count}  -> {error, Count, State}
  end.

-spec find_all(
  sumo:schema_name(), state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName,
         #state{conn = Conn, bucket = Bucket, read_quorum = Rq} = State) ->
  Get = fun({C, B, Kst}, Acc) ->
          fetch_map_bulk(DocName, C, B, Kst, Rq) ++ Acc
        end,
  case stream_keys(Conn, Bucket, Get, []) of
    {ok, Docs} -> {ok, Docs, State};
    {_, Docs}  -> {error, Docs, State}
  end.

-spec find_all(
  sumo:schema_name(),
  term(),
  non_neg_integer(),
  non_neg_integer(),
  state()
) -> sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, _SortFields, Limit, Offset, State) ->
  %% @todo implement search with sort parameters.
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
        Limit,
        Offset,
        #state{conn = Conn,
               bucket = Bucket,
               index = Index,
               read_quorum = Rq} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case fetch_map(Conn, Bucket, iolist_to_binary(Key), Rq) of
        {ok, RMap} ->
          Val = rmap_to_doc(DocName, RMap),
          {ok, [Val], State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case search_docs_by(DocName, Conn, Index, Query, Limit, Offset) of
        {ok, {_, Res}} -> {ok, Res, State};
        {error, Error} -> {error, Error, State}
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

-spec create_schema(
  sumo:schema(), state()
) -> sumo_store:result(state()).
create_schema(_Schema, State) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
doc_id(Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  sumo_internal:get_field(IdField, Doc).

%% @private
new_doc(Doc, #state{conn = Conn, bucket = Bucket, write_quorum = W}) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = case sumo_internal:get_field(IdField, Doc) of
         undefined ->
           case update_map(Conn, Bucket, undefined, doc_to_rmap(Doc), W) of
             {ok, RiakMapId} -> RiakMapId;
             {error, Error}  -> throw(Error);
             _               -> throw(unexpected)
           end;
         Id0 ->
           to_bin(Id0)
       end,
  {Id, sumo_internal:set_field(IdField, Id, Doc)}.

%% @private
%% Support multi-level structures.
doc_to_rmap(Doc) ->
  Fields = sumo_internal:doc_fields(Doc),
  map_to_rmap(Fields).

%% @private
%% Support multi-level structures.
map_to_rmap(Map) ->
  lists:foldl(fun rmap_update/2, riakc_map:new(), maps:to_list(Map)).

%% @private
rmap_update({K, V}, RMap) when is_map(V) ->
  NewV = map_to_rmap(V),
  riakc_map:update({to_bin(K), map}, fun(_M) -> NewV end, RMap);
rmap_update({K, V}, RMap) when is_list(V) ->
  case io_lib:printable_list(V) of
    true ->
      riakc_map:update(
        {to_bin(K), register},
        fun(R) -> riakc_register:set(to_bin(V), R) end,
        RMap);
    false ->
      riakc_map:update({to_bin(K), set}, fun(_S) -> V end, RMap)
  end;
rmap_update({K, V}, RMap) ->
  riakc_map:update(
    {to_bin(K), register},
    fun(R) -> riakc_register:set(to_bin(V), R) end,
    RMap).

%% @private
%% Support multi-level structures.
rmap_to_doc(DocName, RMap) ->
  sumo_internal:new_doc(DocName, rmap_to_map(RMap)).

%% @private
%% Support multi-level structures.
rmap_to_map(RMap) ->
  F = fun({{K, map}, V}, Acc) ->
        maps:put(to_atom(K), rmap_to_map(V), Acc);
      ({{K, _}, V}, Acc) ->
        maps:put(to_atom(K), V, Acc)
      end,
  lists:foldl(F, #{}, riakc_map:value(RMap)).

%% @private
kv_to_doc(DocName, KV) ->
  F = fun({K, V}, Acc) ->
        NK = normalize_doc_fields(K),
        sumo_internal:set_field(to_atom(NK), V, Acc)
      end,
  lists:foldl(F, sumo_internal:new_doc(DocName), KV).

%% @private
normalize_doc_fields(Src) ->
  re:replace(
    Src, <<"_register|_set|_counter|_flag|_map">>, <<"">>,
    [{return, binary}, global]).

%% @private
fetch_map(Conn, Bucket, Key, Opts) ->
  riakc_pb_socket:fetch_type(Conn, Bucket, Key, Opts).

%% @private
fetch_map_bulk(DocName, Conn, Bucket, Keys, Opts) ->
  Fun = fun(K, Acc) ->
          case fetch_map(Conn, Bucket, K, Opts) of
            {ok, M} -> [rmap_to_doc(DocName, M) | Acc];
            _       -> Acc
          end
        end,
  lists:foldl(Fun, [], Keys).

%% @private
delete_map(Conn, Bucket, Key) ->
  riakc_pb_socket:delete(Conn, Bucket, Key).

%% @private
update_map(Conn, Bucket, Key, Map, Opts) ->
  riakc_pb_socket:update_type(Conn, Bucket, Key, riakc_map:to_op(Map), Opts).

%% @private
search(Conn, Index, Query, 0, 0) ->
  riakc_pb_socket:search(Conn, Index, Query);
search(Conn, Index, Query, Limit, Offset) ->
  riakc_pb_socket:search(Conn, Index, Query, [{start, Offset}, {rows, Limit}]).

%% @private
stream_keys(Conn, Bucket, F, Acc) ->
  {ok, Ref} = riakc_pb_socket:get_index_eq(
    Conn, Bucket, <<"$bucket">>, <<"">>, [{stream, true}]),
  receive_stream(Ref, Conn, Bucket, F, Acc).

%% @private
receive_stream(Ref, Conn, Bucket, F, Acc) ->
  receive
    {Ref, {_, Stream, _}} ->
      receive_stream(Ref, Conn, Bucket, F, F({Conn, Bucket, Stream}, Acc));
    {Ref, {done, _}} ->
      {ok, Acc};
    _ ->
      {error, Acc}
  after
    30000 -> {timeout, Acc}
  end.

%% @private
%% @todo Add multi-level support.
%%       Instead of transform search result to doc, get just the key and
%%       fetch the value (`fetch_map`), then apply `rmap_to_doc` on that
%%       value, since this function supports multi-level.
search_docs_by(DocName, Conn, Index, Query, Limit, Offset) ->
  case search(Conn, Index, Query, Limit, Offset) of
    {ok, {search_results, Results, _, Total}} ->
      F = fun({_, KV}, Acc) -> [kv_to_doc(DocName, KV) | Acc] end,
      NewRes = lists:foldl(F, [], Results),
      {ok, {Total, NewRes}};
    {error, Error} ->
      {error, Error}
  end.

%% @private
delete_docs(Conn, Bucket, Docs) ->
  F = fun(D) ->
        K = doc_id(D),
        delete_map(Conn, Bucket, K)
      end,
  lists:foreach(F, Docs).

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

%% @private
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API - Query Builder.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
build_query({q_str, Q}) when is_binary(Q) ->
  Q;
build_query([]) ->
  <<"*:*">>;
build_query(PL) when is_list(PL) ->
  build_query1(PL, <<"">>);
build_query(_) ->
  <<"*:*">>.

%% @private
build_query1([], Acc) ->
  Acc;
build_query1([{_, [{_, _} | _T0]} = KV | T], <<"">>) ->
  build_query1(T,
    <<(<<"(">>)/binary, (build_query2(KV))/binary, (<<")">>)/binary>>);
build_query1([{_, [{_, _} | _T0]} = KV | T], Acc) ->
  build_query1(T,
    <<Acc/binary, (<<" AND (">>)/binary,
      (build_query2(KV))/binary, (<<")">>)/binary>>);
build_query1([{K, V} | T], <<"">>) ->
  build_query1(T, <<(query_eq(K, V))/binary>>);
build_query1([{K, V} | T], Acc) ->
  build_query1(T,
    <<Acc/binary, (<<" AND ">>)/binary, (query_eq(K, V))/binary>>).

%% @private
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
query_eq(K, V) ->
  <<(atom_to_binary(K, utf8))/binary,
    (<<"_register:">>)/binary,
    (to_bin(V))/binary>>.
