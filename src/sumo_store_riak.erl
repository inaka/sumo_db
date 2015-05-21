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

-include_lib("riakc/include/riakc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public API.
-export([init/1]).
-export([create_schema/2]).
-export([persist/2]).
-export([delete_by/3, delete_all/2]).
-export([find_all/2, find_all/5, find_by/3, find_by/5, find_by/6]).

%% Utilities
-export([doc_to_rmap/1, map_to_rmap/1, rmap_to_doc/2, rmap_to_map/1]).
-export([fetch_map/4, fetch_docs/5, delete_map/4, update_map/5]).
-export([search/5, build_query/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Riak base parameters
-type connection() :: pid().
-type index()      :: binary().
-type options()    :: [proplists:property()].

-export_type([connection/0, index/0, options/0]).

%% conn: is the Pid of the gen_server that holds the connection with Riak
%% bucket: Riak bucket (per store)
%% index: Riak index to be used by Riak Search
%% read_quorum: Riak read quorum parameters.
%% write_quorum: Riak write quorum parameters.
%% @see <a href="http://docs.basho.com/riak/latest/dev/using/basics"/>
-record(state, {conn     :: connection(),
                bucket   :: bucket(),
                index    :: index(),
                get_opts :: get_options(),
                put_opts :: put_options(),
                del_opts :: delete_options()}).
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
  GetOpts = proplists:get_value(get_options, Opts, []),
  PutOpts = proplists:get_value(put_options, Opts, []),
  DelOpts = proplists:get_value(delete_options, Opts, []),
  State = #state{conn = Conn,
                 bucket = {BucketType, Bucket},
                 index = Index,
                 get_opts = GetOpts,
                 put_opts = PutOpts,
                 del_opts = DelOpts},
  {ok, State}.

-spec persist(
  sumo_internal:doc(), state()
) -> sumo_store:result(sumo_internal:doc(), state()).
persist(Doc,
        #state{conn = Conn, bucket = Bucket, put_opts = Opts} = State) ->
  {Id, NewDoc} = new_doc(Doc, State),
  case update_map(Conn, Bucket, Id, doc_to_rmap(NewDoc), Opts) of
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
          #state{conn = Conn,
                 bucket = Bucket,
                 index = Index,
                 del_opts = Opts} = State) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case delete_map(Conn, Bucket, to_bin(Key), Opts) of
        ok ->
          {ok, 1, State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case search_docs_by(DocName, Conn, Index, Query, 0, 0) of
        {ok, {Total, Res}}  ->
          delete_docs(Conn, Bucket, Res, Opts),
          {ok, Total, State};
        {error, Error} ->
          {error, Error, State}
      end
  end.

-spec delete_all(
  sumo:schema_name(), state()
) -> sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(_DocName,
           #state{conn = Conn, bucket = Bucket, del_opts = Opts} = State) ->
  Del = fun(Kst, Acc) ->
          lists:foreach(fun(K) -> delete_map(Conn, Bucket, K, Opts) end, Kst),
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
         #state{conn = Conn, bucket = Bucket, get_opts = Opts} = State) ->
  Get = fun(Kst, Acc) ->
          fetch_docs(DocName, Conn, Bucket, Kst, Opts) ++ Acc
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
find_by(DocName, Conditions, Limit, Offset,
        #state{conn = Conn,
               bucket = Bucket,
               index = Index,
               get_opts = Opts} = State) when is_list(Conditions) ->
  IdField = sumo_internal:id_field_name(DocName),
  case lists:keyfind(IdField, 1, Conditions) of
    {_K, Key} ->
      case fetch_map(Conn, Bucket, to_bin(Key), Opts) of
        {ok, RMap} ->
          Val = rmap_to_doc(DocName, RMap),
          {ok, [Val], State};
        {error, {notfound, _}} ->
          {ok, [], State};
        {error, Error} ->
          {error, Error, State}
      end;
    _ ->
      Query = build_query(Conditions),
      case search_docs_by(DocName, Conn, Index, Query, Limit, Offset) of
        {ok, {_, Res}} -> {ok, Res, State};
        {error, Error} -> {error, Error, State}
      end
  end;
find_by(DocName, Conditions, Limit, Offset,
        #state{conn = Conn, index = Index} = State) ->
  Query = build_query(Conditions),
  case search_docs_by(DocName, Conn, Index, Query, Limit, Offset) of
    {ok, {_, Res}} -> {ok, Res, State};
    {error, Error} -> {error, Error, State}
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
%% Utilities API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec doc_to_rmap(sumo_internal:doc()) -> riakc_map:crdt_map().
doc_to_rmap(Doc) ->
  Fields = sumo_internal:doc_fields(Doc),
  map_to_rmap(Fields).

-spec map_to_rmap(map()) -> riakc_map:crdt_map().
map_to_rmap(Map) ->
  lists:foldl(fun rmap_update/2, riakc_map:new(), maps:to_list(Map)).

-spec rmap_to_doc(
  sumo:schema_name(), riakc_map:crdt_map()
) -> sumo_internal:doc().
rmap_to_doc(DocName, RMap) ->
  sumo_internal:new_doc(DocName, rmap_to_map(RMap)).

-spec rmap_to_map(riakc_map:crdt_map()) -> map().
rmap_to_map(RMap) ->
  F = fun({{K, map}, V}, Acc) ->
        maps:put(to_atom(K), rmap_to_map({map, V, [], [], undefined}), Acc);
      ({{K, _}, V}, Acc) ->
        maps:put(to_atom(K), V, Acc)
      end,
  lists:foldl(F, #{}, riakc_map:value(RMap)).

-spec fetch_map(
  connection(), bucket(), key(), options()
) -> {ok, riakc_datatype:datatype()} | {error, term()}.
fetch_map(Conn, Bucket, Key, Opts) ->
  riakc_pb_socket:fetch_type(Conn, Bucket, Key, Opts).

-spec fetch_docs(
  sumo:schema_name(), connection(), bucket(), [key()], options()
) -> [sumo_internal:doc()].
fetch_docs(DocName, Conn, Bucket, Keys, Opts) ->
  Fun = fun(K, Acc) ->
          case fetch_map(Conn, Bucket, K, Opts) of
            {ok, M} -> [rmap_to_doc(DocName, M) | Acc];
            _       -> Acc
          end
        end,
  lists:foldl(Fun, [], Keys).

-spec delete_map(
  connection(), bucket(), key(), options()
) -> ok | {error, term()}.
delete_map(Conn, Bucket, Key, Opts) ->
  riakc_pb_socket:delete(Conn, Bucket, Key, Opts).

-spec update_map(
  connection(), bucket(), key() | undefined, riakc_map:crdt_map(), options()
) ->
  ok | {ok, Key::binary()} | {ok, riakc_datatype:datatype()} |
  {ok, Key::binary(), riakc_datatype:datatype()} | {error, term()}.
update_map(Conn, Bucket, Key, Map, Opts) ->
  riakc_pb_socket:update_type(Conn, Bucket, Key, riakc_map:to_op(Map), Opts).

-spec search(
  connection(), index(), binary(), non_neg_integer(), non_neg_integer()
) -> {ok, search_result()} | {error, term()}.
search(Conn, Index, Query, 0, 0) ->
  riakc_pb_socket:search(Conn, Index, Query);
search(Conn, Index, Query, Limit, Offset) ->
  riakc_pb_socket:search(Conn, Index, Query, [{start, Offset}, {rows, Limit}]).

-spec build_query(sumo:conditions()) -> binary().
build_query(Conditions) ->
  build_query1(Conditions, fun escape/1, fun quote/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
doc_id(Doc) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  sumo_internal:get_field(IdField, Doc).

%% @private
new_doc(Doc, #state{conn = Conn, bucket = Bucket, put_opts = Opts}) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = case sumo_internal:get_field(IdField, Doc) of
         undefined ->
           case update_map(Conn, Bucket, undefined, doc_to_rmap(Doc), Opts) of
             {ok, RiakMapId} -> RiakMapId;
             {error, Error}  -> throw(Error);
             _               -> throw(unexpected)
           end;
         Id0 ->
           to_bin(Id0)
       end,
  {Id, sumo_internal:set_field(IdField, Id, Doc)}.

%% @private
list_to_rset(_, [], Acc) ->
  Acc;
list_to_rset(K, [H | T], Acc) ->
  M = riakc_map:update(
    {to_bin(K), set},
    fun(S) -> riakc_set:add_element(to_bin(H), S) end,
    Acc),
  list_to_rset(K, T, M).

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
      list_to_rset(K, V, RMap)
  end;
rmap_update({K, V}, RMap) ->
  riakc_map:update(
    {to_bin(K), register},
    fun(R) -> riakc_register:set(to_bin(V), R) end,
    RMap).

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
stream_keys(Conn, Bucket, F, Acc) ->
  {ok, Ref} = riakc_pb_socket:get_index_eq(
    Conn, Bucket, <<"$bucket">>, <<"">>, [{stream, true}]),
  receive_stream(Ref, F, Acc).

%% @private
receive_stream(Ref, F, Acc) ->
  receive
    {Ref, {_, Stream, _}} ->
      receive_stream(Ref, F, F(Stream, Acc));
    {Ref, {done, _}} ->
      {ok, Acc};
    _ ->
      {error, Acc}
  after
    30000 -> {timeout, Acc}
  end.

%% @private
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
delete_docs(Conn, Bucket, Docs, Opts) ->
  F = fun(D) ->
        K = doc_id(D),
        delete_map(Conn, Bucket, K, Opts)
      end,
  lists:foreach(F, Docs).

%% @private
to_bin(Data) when is_integer(Data) ->
  <<Data:128/integer>>;
to_bin(Data) when is_float(Data) ->
  <<Data/float>>;
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
build_query1([], _EscapeFun, _QuoteFun) ->
  <<"*:*">>;
build_query1(Exprs, EscapeFun, QuoteFun) when is_list(Exprs) ->
  Clauses = [build_query1(Expr, EscapeFun, QuoteFun) || Expr <- Exprs],
  binary:list_to_bin(["(", interpose(" AND ", Clauses), ")"]);
build_query1({'and', Exprs}, EscapeFun, QuoteFun) ->
  build_query1(Exprs, EscapeFun, QuoteFun);
build_query1({'or', Exprs}, EscapeFun, QuoteFun) ->
  Clauses = [build_query1(Expr, EscapeFun, QuoteFun) || Expr <- Exprs],
  binary:list_to_bin(["(", interpose(" OR ", Clauses), ")"]);
build_query1({'not', Expr}, EscapeFun, QuoteFun) ->
  binary:list_to_bin(["(NOT ", build_query1(Expr, EscapeFun, QuoteFun), ")"]);
build_query1({Name, '<', Value}, EscapeFun, _QuoteFun) ->
  NewVal = binary:list_to_bin(["{* TO ", EscapeFun(Value), "}"]),
  query_eq(Name, NewVal);
build_query1({Name, '<=', Value}, EscapeFun, _QuoteFun) ->
  NewVal = binary:list_to_bin(["[* TO ", EscapeFun(Value), "]"]),
  query_eq(Name, NewVal);
build_query1({Name, '>', Value}, EscapeFun, _QuoteFun) ->
  NewVal = binary:list_to_bin(["{", EscapeFun(Value), " TO *}"]),
  query_eq(Name, NewVal);
build_query1({Name, '>=', Value}, EscapeFun, _QuoteFun) ->
  NewVal = binary:list_to_bin(["[", EscapeFun(Value), " TO *]"]),
  query_eq(Name, NewVal);
build_query1({Name, '==', Value}, EscapeFun, QuoteFun) ->
  build_query1({Name, Value}, EscapeFun, QuoteFun);
build_query1({Name, '/=', Value}, EscapeFun, QuoteFun) ->
  build_query1({negative_field(Name), Value}, EscapeFun, QuoteFun);
build_query1({Name, 'like', Value}, _EscapeFun, _QuoteFun) ->
  NewVal = like_to_wildcard_search(Value),
  Bypass = fun(X) -> X end,
  build_query1({Name, NewVal}, Bypass, Bypass);
build_query1({Name, 'null'}, _EscapeFun, _QuoteFun) ->
  %% null: (Field:undefined OR (NOT Field:[* TO *]))
  Val = {'or', [{Name, <<"undefined">>}, {'not', {Name, <<"[* TO *]">>}}]},
  Bypass = fun(X) -> X end,
  build_query1(Val, Bypass, Bypass);
build_query1({Name, 'not_null'}, _EscapeFun, _QuoteFun) ->
  %% not_null: (Field:[* TO *] AND -Field:undefined)
  Val = {'and', [{Name, <<"[* TO *]">>}, {Name, '/=', <<"undefined">>}]},
  Bypass = fun(X) -> X end,
  build_query1(Val, Bypass, Bypass);
build_query1({Name, Value}, EscapeFun, QuoteFun) ->
  query_eq(Name, QuoteFun(EscapeFun(Value))).

%% @private
query_eq(K, V) ->
  binary:list_to_bin([build_key(K), V]).

%% @private
build_key(K) ->
  build_key(binary:split(to_bin(K), <<".">>, [global]), <<"">>).

%% @private
build_key([K], <<"">>) ->
  binary:list_to_bin([K, "_register:"]);
build_key([K], Acc) ->
  binary:list_to_bin([Acc, ".", K, "_register:"]);
build_key([K | T], <<"">>) ->
  build_key(T, binary:list_to_bin([K, "_map"]));
build_key([K | T], Acc) ->
  build_key(T, binary:list_to_bin([Acc, ".", K, "_map"])).

%% @private
interpose(Sep, List) ->
  interpose(Sep, List, []).

%% @private
interpose(_Sep, [], Result) ->
  lists:reverse(Result);
interpose(Sep, [Item | []], Result) ->
  interpose(Sep, [], [Item | Result]);
interpose(Sep, [Item | Rest], Result) ->
  interpose(Sep, Rest, [Sep, Item | Result]).

%% @private
negative_field(Name) ->
  binary:list_to_bin([<<"-">>, to_bin(Name)]).

%% @private
quote(Value) ->
  [$\", re:replace(to_bin(Value), "[\\\"\\\\]", "\\\\&", [global]), $\"].

%% @private
escape(Value) ->
  Escape = "[\\+\\-\\&\\|\\!\\(\\)\\{\\}\\[\\]\\^\\\"\\~\\*\\?\\:\\\\]",
  re:replace(to_bin(Value), Escape, "\\\\&", [global, {return, binary}]).

%% @private
whitespace(Value) ->
  re:replace(Value, "[\\\s\\\\]", "\\\\&", [global, {return, binary}]).

%% @private
like_to_wildcard_search(Like) ->
  whitespace(binary:replace(to_bin(Like), <<"%">>, <<"*">>, [global])).
