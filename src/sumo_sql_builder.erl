%%% @doc Small and simple SQL builder.
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
-module(sumo_sql_builder).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-type field() :: string() | atom().
-type condition() ::
  {'and', [condition()]} | {'or', [condition()]} | {field(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([i/2, u/3, d/2]).
-export([s/7, s_count/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns number of results, useful for pagination.
-spec s_count(string(), [field()], condition(), string()) -> {iolist(), [term()]}.
s_count(TableName, SelectFields, Conditions, ExtraWhere) ->
  {_Select, Where, WValues} = form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT COUNT(1) AS `count` FROM ", escape(TableName), " WHERE ", Where],
    WValues
  }.

%% @doc Generic select function.
-spec s(
  string(), [field()], condition(), string(), pos_integer(), pos_integer(), string()
) -> {iolist(), [term()]}.
s(TableName, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) ->
  Paging = [" LIMIT ", integer_to_list((Page-1) * PageSize), ",", integer_to_list(PageSize)],
  {Select, Where, WValues} = form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT ", Select, " FROM ", escape(TableName), " WHERE ", Where, " ", OrderBy, " ", Paging],
    WValues
  }.

%% @doc INSERT.
-spec i(string(), proplists:proplist()) -> {iolist(), [term()]}.
i(TableName, Proplist) ->
  {Fields, Values, Args} = lists:foldr(
    fun({K, V}, {Fs, Vs, Args}) ->
      {[escape(K)|Fs], [V|Vs], ["?"|Args]}
    end,
    {[], [], []},
    Proplist
  ),
  {
    [
      "INSERT INTO ", escape(TableName), " (", string:join(Fields, ","), ") ",
      "VALUES (", string:join(Args, ","),")"
    ],
    Values
  }.

%% @doc UPDATE.
-spec u(
  string(), proplists:proplist(), condition()
) -> {iolist(), [term()], [term()]}.
u(TableName, UpdateFields, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {UFields, UValues} = lists:foldr(
    fun({K, V}, {Fs, Vs}) ->
      {[escape(K) ++ "=?"|Fs], [V|Vs]}
    end,
    {[], []},
    UpdateFields
  ),
  Update = string:join(UFields, ","),
  {["UPDATE ", escape(TableName), " SET ", Update, " WHERE ", Where], UValues, WValues}. 

%% @doc DELETE.
-spec d(string(), condition()) -> {iolist(), [term()]}.
d(TableName, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {["DELETE FROM ", escape(TableName), " WHERE ", Where], WValues}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Query generator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec escape(field()) -> string().
escape(Field) when is_atom(Field) ->
  escape(atom_to_list(Field));

escape(Field) when is_list(Field) ->
  lists:flatten(["`", Field, "`"]).

-spec form_select_query([field()], condition(), string()) -> {string(), string(), [string()]}.
form_select_query(SelectFields, Conditions, ExtraWhere) ->
  WhereTmp = form_condition(Conditions),
  WValues = lists:reverse(condition_values([Conditions])),
  SFields = [escape(F) || F <- SelectFields],
  Where = case ExtraWhere of
    [] -> WhereTmp;
    ExtraWhere -> [WhereTmp, case WhereTmp of [] -> " "; _ -> " AND " end, ExtraWhere]
  end,
  Select = string:join(SFields, ","),
  % SelectedFields, Where clause, and Where values
  {Select, Where, WValues}.

form_condition({'and', Conditions}) ->
  join_conditions(Conditions, "AND");

form_condition({'or', Conditions}) ->
  join_conditions(Conditions, "OR");

form_condition({_Key, {any, []}}) ->
  "";

form_condition({Key, {any, Values}}) ->
  Conditions = [{Key, V} || V <- Values],
  form_condition({'or', Conditions});

form_condition({_Key, {_, undefined}}) ->
  "";

form_condition({Key, {le, _Value}}) ->
  lists:flatten(["(", escape(Key), "<=?", ")"]);

form_condition({Key, {gt, _Value}}) ->
  lists:flatten(["(", escape(Key), ">?", ")"]);

form_condition({Key, {ge, _Value}}) ->
  lists:flatten(["(", escape(Key), ">=?", ")"]);

form_condition({Key, {lt, _Value}}) ->
  lists:flatten(["(", escape(Key), "<?", ")"]);

form_condition({_Key, undefined}) ->
  "";

form_condition({Key, {'not', _Value}}) ->
  lists:flatten(["(", escape(Key), "!=?", ")"]);

form_condition({Key, is_null}) ->
  lists:flatten(["(", escape(Key), " IS NULL", ")"]);

form_condition({Key, not_null}) ->
  lists:flatten(["(", escape(Key), " IS NOT NULL", ")"]);

form_condition({Key, _Value}) ->
  lists:flatten(["(", escape(Key), "=?", ")"]).

join_conditions([], _Operator) ->
  "";

join_conditions(Conditions, Operator) ->
  FormedConditions = [form_condition(C) || C <- Conditions],
  WithoutEmptyConditions = [C || C <- FormedConditions, C =/= ""],
  case WithoutEmptyConditions of
    [] -> "";
    _ -> lists:flatten(["(", string:join(WithoutEmptyConditions, " " ++ Operator ++ " "), ")"])
  end.

condition_values(Conditions) ->
  condition_values(Conditions, []).

condition_values([], Acc) ->
  Acc;

condition_values([{'and', Conditions}|Rest], Acc) ->
  NewAcc = condition_values(Conditions, Acc),
  condition_values(Rest, NewAcc);

condition_values([{'or', Conditions}|Rest], Acc) ->
  NewAcc = condition_values(Conditions, Acc),
  condition_values(Rest, NewAcc);

condition_values([{_Key, {any, Values}}|Rest], Acc) ->
  NewAcc = lists:foldl(fun(V, InnerAcc) -> [V|InnerAcc] end, Acc, Values),
  condition_values(Rest, NewAcc);

condition_values([{_Key, {gt, Value}}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]);

condition_values([{_Key, {lt, Value}}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]);

condition_values([{_Key, {ge, Value}}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]);

condition_values([{_Key, {le, Value}}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]);

condition_values([{_Key, {'not', Value}}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]);

condition_values([{_Key, is_null}|Rest], Acc) ->
  condition_values(Rest, Acc);

condition_values([{_Key, not_null}|Rest], Acc) ->
  condition_values(Rest, Acc);

condition_values([{_Key, Value}|Rest], Acc) ->
  condition_values(Rest, [Value|Acc]).
