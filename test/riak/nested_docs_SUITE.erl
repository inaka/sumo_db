-module(nested_docs_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2
        ]).

-export([
         find_all/1,
         find_by/1,
         update/1,
         delete_all/1,
         delete/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  [find_all, find_by, update, delete_all, delete].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sumo_db),
  Config.

init_per_testcase(_, Config) ->
  init_store(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(sumo_test_purchase_order),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_all(_Config) ->
  11 = length(sumo:find_all(sumo_test_purchase_order)),
  All1 = sumo:find_all(sumo_test_purchase_order, [], 2, 0),
  2 = length(All1),
  All2 = sumo:find_all(sumo_test_purchase_order, [], 10, 2),
  9 = length(All2),
  ok.

find_by(_Config) ->
  Results1 = sumo:find_by(sumo_test_purchase_order, [{currency, <<"USD">>}]),
  2 = length(Results1),

  [ #{id        := <<"ID1">>,
      currency  := <<"USD">>,
      items     := [#{part_num := <<"123">>}, #{part_num := <<"456">>}],
      order_num := <<"O1">>,
      ship_to   := #{city := <<"city1">>, country := <<"US">>},
      bill_to   := #{city := <<"city1">>, country := <<"US">>},
      total     := 300},
    #{id        := <<"ID2">>,
      currency  := <<"USD">>,
      items     := [#{part_num := <<"123">>}, #{part_num := <<"456">>}],
      order_num := <<"O2">>,
      ship_to   := #{city := <<"city1">>, country := <<"US">>},
      bill_to   := #{city := <<"city1">>, country := <<"US">>},
      total     := 300}
  ] = Results1,

  Results2 = sumo:find_by(sumo_test_purchase_order, [{currency, <<"EUR">>}]),
  1 = length(Results2),

  [ #{id        := <<"ID3">>,
      currency  := <<"EUR">>,
      items     := [#{part_num := <<"123">>}, #{part_num := <<"456">>}],
      order_num := <<"O3">>,
      ship_to   := #{city := <<"city2">>, country := <<"US">>},
      bill_to   := #{city := <<"city1">>, country := <<"US">>},
      total     := 300}
  ] = Results2,

  PO1 = sumo:find(sumo_test_purchase_order, <<"ID1">>),
  #{id        := <<"ID1">>,
    currency  := <<"USD">>,
    items     := [#{part_num := <<"123">>}, #{part_num := <<"456">>}],
    order_num := <<"O1">>,
    ship_to   := #{city := <<"city1">>, country := <<"US">>},
    bill_to   := #{city := <<"city1">>, country := <<"US">>},
    total     := 300} = PO1,

  notfound = sumo:find(sumo_test_purchase_order, <<"ID123">>),

  Results3 = sumo:find_by(
    sumo_test_purchase_order, [{'ship_to.city', <<"city2">>}]),
  1 = length(Results3),
  Results2 = Results3,

  Results4 = sumo:find_by(
    sumo_test_purchase_order,
    [{'ship_to.city', <<"city2">>}, {currency, <<"USD">>}]),
  0 = length(Results4),

  Results5 = sumo:find_by(
    sumo_test_purchase_order,
    [{'ship_to.city', <<"city1">>}, {currency, <<"USD">>}]),
  2 = length(Results5),

  ok.

update(_Config) ->
  PO1 = sumo:find(sumo_test_purchase_order, <<"ID1">>),

  PO1x = sumo_test_purchase_order:order_num(PO1, <<"0001">>),
  sumo:persist(sumo_test_purchase_order, PO1x),

  PO1x = sumo:find(sumo_test_purchase_order, <<"ID1">>),

  PO1y = sumo_test_purchase_order:order_num(PO1x, <<"00011">>),
  sumo:persist(sumo_test_purchase_order, PO1y),

  PO1y = sumo:find(sumo_test_purchase_order, <<"ID1">>),

  ok.

delete_all(_Config) ->
  sumo:delete_all(sumo_test_purchase_order),
  [] = sumo:find_all(sumo_test_purchase_order).

delete(_Config) ->
  %% delete_by
  2  = sumo:delete_by(sumo_test_purchase_order, [{currency, <<"USD">>}]),
  sync_timeout(9),
  [] = sumo:find_by(sumo_test_purchase_order, [{currency, <<"USD">>}]),

  %% delete
  sumo:delete(sumo_test_purchase_order, <<"ID3">>),
  8 = length(sumo:find_all(sumo_test_purchase_order)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_store() ->
  sumo:create_schema(sumo_test_purchase_order),
  sumo:delete_all(sumo_test_purchase_order),
  sync_timeout(0),

  Addr1 = sumo_test_purchase_order:new_address(<<"l1">>, <<"l2">>, <<"city1">>,
                                               <<"s">>, <<"zip">>, <<"US">>),
  Addr2 = sumo_test_purchase_order:new_address(<<"l1">>, <<"l2">>, <<"city2">>,
                                               <<"s">>, <<"zip">>, <<"US">>),
  Addr3 = sumo_test_purchase_order:new_address(<<"l1">>, <<"l2">>, <<"city3">>,
                                               <<"s">>, <<"zip">>, <<"US">>),

  Item1 = sumo_test_purchase_order:new_item(<<"123">>, <<"p1">>, 1, 100, 100),
  Item2 = sumo_test_purchase_order:new_item(<<"456">>, <<"p2">>, 2, 100, 200),
  Items = [Item1, Item2],

  Date = calendar:universal_time(),

  PO1 = sumo_test_purchase_order:new(<<"ID1">>,  <<"O1">>, Date, Addr1, Addr1,
                                     Items, <<"USD">>, 300),
  PO2 = sumo_test_purchase_order:new(<<"ID2">>,  <<"O2">>, Date, Addr1, Addr1,
                                     Items, <<"USD">>, 300),
  PO3 = sumo_test_purchase_order:new(<<"ID3">>,  <<"O3">>, Date, Addr2, Addr1,
                                     Items, <<"EUR">>, 300),
  PO4 = sumo_test_purchase_order:new(<<"ID4">>,  <<"O4">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  PO5 = sumo_test_purchase_order:new(<<"ID5">>,  <<"O5">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  PO6 = sumo_test_purchase_order:new(<<"ID6">>,  <<"O6">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  PO7 = sumo_test_purchase_order:new(<<"ID7">>,  <<"O7">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  PO8 = sumo_test_purchase_order:new(<<"ID8">>,  <<"O8">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  PO9 = sumo_test_purchase_order:new(<<"ID9">>,  <<"O9">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  P10 = sumo_test_purchase_order:new(<<"ID10">>, <<"10">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),
  P11 = sumo_test_purchase_order:new(<<"ID11">>, <<"11">>, Date, Addr3, Addr3,
                                     Items, <<"ARG">>, 400),

  sumo:persist(sumo_test_purchase_order, PO1),
  sumo:persist(sumo_test_purchase_order, PO2),
  sumo:persist(sumo_test_purchase_order, PO3),
  sumo:persist(sumo_test_purchase_order, PO4),
  sumo:persist(sumo_test_purchase_order, PO5),
  sumo:persist(sumo_test_purchase_order, PO6),
  sumo:persist(sumo_test_purchase_order, PO7),
  sumo:persist(sumo_test_purchase_order, PO8),
  sumo:persist(sumo_test_purchase_order, PO9),
  sumo:persist(sumo_test_purchase_order, P10),
  sumo:persist(sumo_test_purchase_order, P11),

  sync_timeout(11).

sync_timeout(Len) ->
  timer:sleep(5000),
  Len = length(sumo:find_by(sumo_test_purchase_order, [])).
  %Fun = fun() -> length(sumo:find_by(sumo_test_purchase_order, [])) end,
  %ktn_task:wait_for(Fun, Len, 500, 10).
