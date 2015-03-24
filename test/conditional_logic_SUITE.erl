-module(conditional_logic_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         mysql_conditional_logic/1,
         mongo_conditional_logic/1,
         pgsql_conditional_logic/1,
         mnesia_conditional_logic/1
        ]).

-export([
         backward_compatibility/1,
         or_conditional/1,
         and_conditional/1,
         not_null_conditional/1,
         null_conditional/1,
         operators/1,
         deeply_nested/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-define(LOGIC_FUNS,
        [
         backward_compatibility,
         or_conditional,
         and_conditional,
         not_null_conditional,
         null_conditional,
         operators,
         deeply_nested
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS ++ ?LOGIC_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(emongo),
  application:ensure_all_started(emysql),
  application:ensure_all_started(epgsql),
  application:ensure_all_started(mnesia),
  application:ensure_all_started(sumo_db),

  Fun =
    fun (Module) ->
        sumo:create_schema(Module),
        sumo:delete_all(Module),

        sumo:persist(Module, Module:new("Jane", "Doe")),
        sumo:persist(Module, Module:new("John", "Doe", 30)),
        sumo:persist(Module, Module:new("Jane Jr.", "Doe", 5)),
        sumo:persist(Module, Module:new("Joe", "Armstrong")),
        sumo:persist(Module, Module:new("Alan", "Turing", 102, "Computer St."))
    end,

  lists:foreach(Fun, [sumo_test_people_mysql,
                      sumo_test_people_mongo,
                      sumo_test_people_pgsql,
                      sumo_test_people_mnesia]),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported Tests Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mysql_conditional_logic(_Config) ->
  Fun = fun(F) -> conditional_logic_SUITE:F(sumo_test_people_mysql) end,
  lists:foreach(Fun, ?LOGIC_FUNS).

mongo_conditional_logic(_Config) ->
  Fun = fun(F) -> conditional_logic_SUITE:F(sumo_test_people_mongo) end,
  lists:foreach(Fun, ?LOGIC_FUNS).

pgsql_conditional_logic(_Config) ->
  Fun = fun(F) -> conditional_logic_SUITE:F(sumo_test_people_pgsql) end,
  lists:foreach(Fun, ?LOGIC_FUNS).

mnesia_conditional_logic(_Config) ->
  Fun = fun(F) -> conditional_logic_SUITE:F(sumo_test_people_mnesia) end,
  lists:foreach(Fun, ?LOGIC_FUNS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backward_compatibility(Module) ->
  [_, _, _, _, _] = sumo:find_all(Module),

  [_, _, _] = sumo:find_by(Module, [{last_name, "Doe"}]),

  [_] = sumo:find_by(Module, [{name, "Jane"}, {last_name, "Doe"}]).

or_conditional(Module) ->
  [_, _, _] = sumo:find_by(Module,
                           {'or', [{name, "John"},
                                   {name, "Joe"},
                                   {name, "Alan"}
                                  ]
                           }
                          ),

  [_, _] = sumo:find_by(Module,
                        [{last_name, "Doe"},
                         {'or', [{name, "Jane"},
                                 {name, "Jane Jr."}
                                ]
                         }
                        ]
                       ).

and_conditional(Module) ->
  [] = sumo:find_by(Module,
                    {'and', [{name, "John"},
                             {name, "Joe"},
                             {name, "Alan"}
                            ]
                    }
                   ),

  [_, _] = sumo:find_by(Module,
                        {'and', [{last_name, "Doe"},
                                 {'or', [{name, "Jane"},
                                         {name, "Jane Jr."}
                                        ]
                                 }
                                ]
                        }
                       ).

not_null_conditional(Module) ->
  [_, _, _] = sumo:find_by(Module, {age, 'not_null'}),

  [_] = sumo:find_by(Module, {address, 'not_null'}).


null_conditional(Module) ->
  [_, _] = sumo:find_by(Module, {age, 'null'}),

  [_, _, _, _] = sumo:find_by(Module, {address, 'null'}).

operators(Module) ->
  [_, _] = sumo:find_by(Module,
                        {'and', [{age, 'not_null'},
                                 {age, '<', 100}
                                ]
                        }),

  [_] = sumo:find_by(Module,
                     {'and', [{age, 'not_null'},
                              {age, '>', 100}
                             ]
                     }),

  [] = sumo:find_by(Module,
                    {'and', [{age, 'not_null'},
                             {age, '>', 102}
                            ]
                    }),

  [_] = sumo:find_by(Module,
                     {'and', [{age, 'not_null'},
                              {age, '>=', 102}
                             ]
                     }),


  [_] = sumo:find_by(Module,
                     {'and', [{age, 'not_null'},
                              {age, '<', 30}
                             ]
                     }),

  [_, _] = sumo:find_by(Module,
                        {'and', [{age, 'not_null'},
                                 {age, '=<', 30}
                                ]
                        }),

  [_, _, _] = sumo:find_by(Module,
                           {'or', [{age, 'null'},
                                   {age, '==', 30}
                                  ]
                           }),

  [_, _] = sumo:find_by(Module,
                        {'and', [{age, 'not_null'},
                                 {age, '/=', 30}
                                ]
                        }),

  [_, _, _, _] = sumo:find_by(Module, {name, 'like', "J%"}),

  [_, _] = sumo:find_by(Module, {'and', [{name, 'like', "Ja%"}]}),

  [_] = sumo:find_by(Module, {name, 'like', "A%"}),

  [_, _] = sumo:find_by(Module, {name, 'like', "%n"}).

deeply_nested(Module) ->
  Conditions = {'or', [{'and', [{age, '>', 100},
                                {address, 'like', "%Ave%"}]},
                       {age, 'null'},
                       {last_name, "Turing"}
                      ]
               },
  [_, _, _] = sumo:find_by(Module, Conditions).
