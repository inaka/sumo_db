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

-export([
         values_conditions/1,
         where_clause/1,
         where_clause/2,
         order_by_clause/1,
         order_by_clause/2
        ]).

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
  string(), [field()], condition(), string(), non_neg_integer(), non_neg_integer(), string()
) -> {iolist(), [term()]}.
s(TableName, SelectFields, Conditions, ExtraWhere, Page, PageSize, OrderBy) ->
  Paging = [" LIMIT ", integer_to_list((Page-1) * PageSize), ",", integer_to_list(PageSize)],
  {Select, Where, WValues} = form_select_query(SelectFields, Conditions, ExtraWhere),
  {
    ["SELECT ", Select, " FROM ", escape(TableName), " WHERE ", Where, " ", OrderBy, " ", Paging],
    WValues
  }.

%% @doc INSERT.
-spec i(string(), sumo:doc()) -> {iolist(), [term()]}.
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
  string(), sumo:doc(), condition()
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
  {Values, CleanConditions} = values_conditions(Conditions),
  WhereTmp = where_clause(CleanConditions),
  SFields = [escape(F) || F <- SelectFields],
  Where = case ExtraWhere of
    [] -> WhereTmp;
    ExtraWhere -> [WhereTmp, case WhereTmp of [] -> " "; _ -> " AND " end, ExtraWhere]
  end,
  Select = string:join(SFields, ","),
  % SelectedFields, Where clause, and Where values
  {Select, Where, Values}.

-spec values_conditions(sumo_internal:expression()) ->
  {[any()], sumo_internal:expression()}.
values_conditions([Expr | RestExprs]) ->
  {Values, CleanExpr} = values_conditions(Expr),
  {ValuesRest, CleanRestExprs} = values_conditions(RestExprs),
  {Values ++ ValuesRest, [CleanExpr | CleanRestExprs]};
values_conditions({LogicalOp, Exprs})
  when (LogicalOp == 'and')
       or (LogicalOp == 'or')
       or (LogicalOp == 'not') ->
  {Values, CleanExprs} = values_conditions(Exprs),
  {Values, {LogicalOp, CleanExprs}};
values_conditions({Name, Op, Value}) when not is_atom(Value) ->
  sumo_internal:check_operator(Op),
  {[Value], {Name, Op, '?'}};
values_conditions({Name1, Op, Name2}) when is_atom(Name2) ->
  sumo_internal:check_operator(Op),
  {[], {Name1, Op, Name2}};
values_conditions({Name, Value})
  when Value =/= 'null', Value =/= 'not_null' ->
  {[Value], {Name, '?'}};
values_conditions({Name, Value}) ->
  {[], {Name, Value}};
values_conditions([]) ->
  {[], []};
values_conditions(Expr) ->
  throw({unsupported_expression, Expr}).

-spec where_clause(sumo_internal:expression()) -> iodata().
where_clause(Exprs) ->
  where_clause(Exprs, fun escape/1).

-spec where_clause(sumo_internal:expression(), fun()) -> iodata().
where_clause(Exprs, EscapeFun) when is_list(Exprs) ->
  Clauses = lists:map(fun(Expr) -> where_clause(Expr, EscapeFun) end, Exprs),
  ["(", interpose(" AND ", Clauses), ")"];
where_clause({'and', Exprs}, EscapeFun) ->
  where_clause(Exprs, EscapeFun);
where_clause({'or', Exprs}, EscapeFun) ->
  Clauses = lists:map(fun(Expr) -> where_clause(Expr, EscapeFun) end, Exprs),
  ["(", interpose(" OR ", Clauses), ")"];
where_clause({'not', Expr}, EscapeFun) ->
  [" NOT ", "(", where_clause(Expr, EscapeFun), ")"];
where_clause({Name, Op, '?'}, EscapeFun) ->
  [EscapeFun(Name), " ", operator_to_string(Op), " ? "];
where_clause({Name1, Op, Name2}, EscapeFun) ->
  [EscapeFun(Name1), " ", operator_to_string(Op), " ", escape(Name2)];
where_clause({Name, '?'}, EscapeFun) ->
  [EscapeFun(Name), " = ? "];
where_clause({Name, 'null'}, EscapeFun) ->
  [EscapeFun(Name), " IS NULL "];
where_clause({Name, 'not_null'}, EscapeFun) ->
  [EscapeFun(Name), " IS NOT NULL "].

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
