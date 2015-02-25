%%% @hidden
%%% @doc convenience functions for defining common map/reduce phases
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
-module(sumo_store_riak_mapred).
-author("Carlos Andres Bolanos <candres.bolanos@inakanetworks.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

%% API
-export([map_object_key/3,
         map_object_value/3,
         map_object_kv/3,
         map_object_key_by_index/3,
         map_object_value_by_index/3,
         map_object_kv_by_index/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API - Map/Reduce Functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec map_object_key(riak_object:riak_object(), term(), term()) -> [term()]
%% @doc map phase function
%%      Return a list that contains the key of each object named by
%%      BucketKeys.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_key({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_key(Object, _KeyData, _Arg) ->
  [return(k, Object)].

%% @spec map_object_value(riak_object:riak_object(), term(), term()) -> [term()]
%% @doc map phase function
%%      Return a list that contains the value of each object named by
%%      BucketKeys.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_value({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_value(Object, _KeyData, _Arg) ->
  [return(v, Object)].

%% @spec map_object_kv(riak_object:riak_object(), term(), term()) -> [term()]
%% @doc map phase function
%%      Return a list that contains the key/value pair of each object named by
%%      BucketKeys.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_kv({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_kv(Object, _KeyData, _Arg) ->
  [return(kv, Object)].

%% @spec map_object_key_by_index(riak_object:riak_object(), term(), term()) ->
%%                              [term()]
%% @doc map phase function
%%      Return a list that contains the key of each object named by
%%      BucketKeys, that match with the specified indices.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_key_by_index({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_key_by_index(Object, KeyData, Arg) ->
  find_by_index(Object, KeyData, Arg, k).

%% @spec map_object_value_by_index(riak_object:riak_object(), term(), term()) ->
%%                                [term()]
%% @doc map phase function
%%      Return a list that contains the value of each object named by
%%      BucketKeys, that match with the specified indices.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_value_by_index({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_value_by_index(Object, KeyData, Arg) ->
  find_by_index(Object, KeyData, Arg, v).

%% @spec map_object_kv_by_index(riak_object:riak_object(), term(), term()) ->
%%                             [term()]
%% @doc map phase function
%%      Return a list that contains the key/value pair of each object named by
%%      BucketKeys, that match with the specified indices.
%%      If the RiakObject is the tuple {error, notfound}, the
%%      behavior of this function is produce no output (literally []).
map_object_kv_by_index({error, notfound}, _KeyData, _Arg) ->
  [];
map_object_kv_by_index(Object, KeyData, Arg) ->
  find_by_index(Object, KeyData, Arg, kv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
find_by_index(O, _, IdxL, Ret) ->
  {struct, Map} = mochijson2:decode(riak_object:get_value(O)),
  Deleted = dict:is_key(<<"X-Riak-Deleted">>, riak_object:get_metadata(O)),
  F = fun({X, Y}, Acc) -> (proplists:get_value(X, Map) =:= Y) and Acc end,
  Eval = lists:foldl(F, not Deleted, IdxL),
  case Eval of
    true  -> [return(Ret, O)];
    false -> []
  end.

%% @private
return(k, O) ->
  riak_object:key(O);
return(v, O) ->
  riak_object:get_value(O);
return(kv, O) ->
  {riak_object:key(O), riak_object:get_value(O)}.
