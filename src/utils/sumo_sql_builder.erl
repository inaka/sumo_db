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

%%% Public API.
-export([i/2, u/3, d/2]).
-export([s/7, s_count/4]).

-export([
  values_conditions/1,
  where_clause/1,
  where_clause/2,
  where_clause/3,
  order_by_clause/1,
  order_by_clause/2
]).

-export([
  escape/1,
  slot_numbered/1,
  slot_question/1
]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type field() :: string() | atom().
-type condition() ::
  {'and', [condition()]} | {'or', [condition()]} | {field(), term()}.

%%%=============================================================================
%%% Public API
%%%=============================================================================

%% @doc Returns number of results, useful for pagination.
-spec s_count(
  string(), [field()], condition(), string()
) -> {iolist(), [term()]}.
s_count(TableName, SelectFields, Conditions, ExtraWhere) ->
  {_Select, Where, WValues} =
    form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT COUNT(1) AS `count` FROM ", escape(TableName), " WHERE ", Where],
    WValues
  }.

%% @doc Generic select function.
-spec s(
  string(), [field()], condition(), string(), non_neg_integer(),
  non_neg_integer(), string()
) -> {iolist(), [term()]}.
s(TableName, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) ->
  Paging =
    [ " LIMIT ", integer_to_list((Page-1) * PageSize), ", ",
      integer_to_list(PageSize)],
  {Select, Where, WValues} =
    form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT ", Select,
     " FROM ", escape(TableName),
     " WHERE ", Where, " ", OrderBy, " ", Paging],
    WValues
  }.

%% @doc INSERT.
-spec i(atom() | string(), proplists:proplist()) -> {iodata(), [term()]}.
i(TableName, Proplist) ->
  {Fields, Values, Args} = lists:foldr(fun({K, V}, {Fs, Vs, Args}) ->
    {[escape(K)|Fs], [V|Vs], ["?"|Args]}
  end, {[], [], []}, Proplist),
  {
    [
     "INSERT INTO ", escape(TableName), " (", string:join(Fields, ", "), ") ",
     "VALUES (", string:join(Args, ", "), ")"
    ],
    Values
  }.

%% @doc UPDATE.
-spec u(
  atom() | string(), proplists:proplist(), condition()
) -> {iodata(), [term()], [term()]}.
u(TableName, UpdateFields, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {UFields, UValues} = lists:foldr(fun({K, V}, {Fs, Vs}) ->
    {[escape(K) ++ "=?"|Fs], [V|Vs]}
  end, {[], []}, UpdateFields),
  Update = string:join(UFields, ","),
  {["UPDATE ", escape(TableName), " SET ", Update, " WHERE ", Where],
   UValues,
   WValues
  }.

%% @doc DELETE.
-spec d(string(), condition()) -> {iolist(), [term()]}.
d(TableName, Conditions) ->
  {_Select, Where, WValues} = form_select_query([], Conditions, ""),
  {["DELETE FROM ", escape(TableName), " WHERE ", Where], WValues}.

%%%=============================================================================
%%% Query generator
%%%=============================================================================

-spec escape(field()) -> string().
escape(Field) when is_atom(Field) ->
  escape(atom_to_list(Field));

escape(Field) when is_list(Field) ->
  lists:flatten(["`", Field, "`"]).

-spec form_select_query([field()], condition(), string()) ->
  {string(), string(), [string()]}.
form_select_query(SelectFields, Conditions, ExtraWhere) ->
  {Values, CleanConditions} = values_conditions(Conditions),
  WhereTmp = where_clause(CleanConditions),
  SFields = [escape(F) || F <- SelectFields],
  Where = case ExtraWhere of
    [] -> WhereTmp;
    ExtraWhere ->
      [WhereTmp, case WhereTmp of [] -> " "; _ -> " AND " end, ExtraWhere]
  end,
  Select = string:join(SFields, ","),
  % SelectedFields, Where clause, and Where values
  {Select, Where, Values}.

-spec values_conditions(sumo:conditions()) ->
  {[any()], sumo:conditions()}.
values_conditions(Expr) ->
  {Values, CleanExprs, _} = values_conditions(Expr, {[], [], 1}),
  {lists:reverse(Values), lists:reverse(CleanExprs)}.

values_conditions(Exprs, Acc) when is_list(Exprs) ->
  lists:foldl(fun values_conditions/2, Acc, Exprs);
values_conditions({LogicalOp, Exprs}, {Values, CleanExprs, Count})
    when (LogicalOp == 'and') or (LogicalOp == 'or') or (LogicalOp == 'not') ->
  {NewValues, NewCleanExprs, NewCount} =
    values_conditions(Exprs, {Values, [], Count}),
  {NewValues,
   [{LogicalOp, lists:reverse(NewCleanExprs)} | CleanExprs],
   NewCount};
values_conditions({Name, Op, Value}, {Values, CleanExprs, Count})
    when not is_atom(Value) ->
  sumo_internal:check_operator(Op),
  {[Value | Values],
   [{Name, Op, {'?', Count}} | CleanExprs],
   Count + 1};
values_conditions({Name1, Op, Name2}, {Values, CleanExprs, Count})
    when is_atom(Name2) ->
  sumo_internal:check_operator(Op),
  {Values,
   [{Name1, Op, Name2} | CleanExprs],
   Count};
values_conditions({Name, Value}, {Values, CleanExprs, Count})
    when Value =/= 'null', Value =/= 'not_null' ->
  {[Value | Values],
   [{Name, {'?', Count}} | CleanExprs],
   Count + 1};
values_conditions({Name, Value}, {Values, CleanExprs, Count}) ->
  {Values,
   [{Name, Value} | CleanExprs],
   Count};
values_conditions([], Acc) ->
  Acc;
values_conditions(Expr, _) ->
  exit({unsupported_expression, Expr}).

-spec where_clause(sumo:conditions()) -> iodata().
where_clause(Exprs) ->
  where_clause(Exprs, fun escape/1, fun slot_question/1).

-spec where_clause(sumo:conditions(), fun()) -> iodata().
where_clause(Exprs, EscapeFun) ->
  where_clause(Exprs, EscapeFun, fun slot_question/1).

-spec where_clause(sumo:conditions(), fun(), fun()) -> iodata().
where_clause([], _EscapeFun, _SlotFun) ->
  [];
where_clause(Exprs, EscapeFun, SlotFun) when is_list(Exprs) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" AND ", Clauses), ")"];
where_clause({'and', Exprs}, EscapeFun, SlotFun) ->
  where_clause(Exprs, EscapeFun, SlotFun);
where_clause({'or', Exprs}, EscapeFun, SlotFun) ->
  Clauses = [where_clause(Expr, EscapeFun, SlotFun) || Expr <- Exprs],
  ["(", interpose(" OR ", Clauses), ")"];
where_clause({'not', Expr}, EscapeFun, SlotFun) ->
  [" NOT ", "(", where_clause(Expr, EscapeFun, SlotFun), ")"];
where_clause({Name, Op, {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " ", operator_to_string(Op), SlotFun(Slot)];
where_clause({Name1, Op, Name2}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name1), " ", operator_to_string(Op), " ", escape(Name2)];
where_clause({Name,  {'?', _} = Slot}, EscapeFun, SlotFun) ->
  [EscapeFun(Name), " = ", SlotFun(Slot)];
where_clause({Name, 'null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NULL "];
where_clause({Name, 'not_null'}, EscapeFun, _SlotFun) ->
  [EscapeFun(Name), " IS NOT NULL "].

-spec slot_question({'?', integer()}) -> string().
slot_question(_) -> " ? ".

-spec slot_numbered({'?', integer()}) -> iodata().
slot_numbered({_, N}) -> [" $", integer_to_list(N), " "].

-spec interpose(term(), list()) -> list().
interpose(Sep, List) ->
  interpose(Sep, List, []).

-spec interpose(term(), list(), list()) -> list().
interpose(_Sep, [], Result) ->
  lists:reverse(Result);
interpose(Sep, [Item | []], Result) ->
  interpose(Sep, [], [Item | Result]);
interpose(Sep, [Item | Rest], Result) ->
  interpose(Sep, Rest, [Sep, Item | Result]).

-spec operator_to_string(atom()) -> string().
operator_to_string('=<') -> "<=";
operator_to_string('/=') -> "!=";
operator_to_string('==') -> "=";
operator_to_string(Op) -> atom_to_list(Op).

-spec order_by_clause([{atom(), sumo:sort_order()}]) -> iolist().
order_by_clause(SortFields) ->
  order_by_clause(SortFields, fun escape/1).

-spec order_by_clause([{atom(), sumo:sort_order()}], fun()) -> iolist().
order_by_clause(SortFields, EscapeFun) ->
  ClauseFun = fun({Name, SortOrder}) ->
    [EscapeFun(atom_to_list(Name)), " ", atom_to_list(SortOrder)]
  end,
  Clauses = lists:map(ClauseFun, SortFields),
  [" ORDER BY ", interpose(", ", Clauses)].
