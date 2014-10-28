-module(conditional_logic_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
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

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(emysql),
  application:ensure_all_started(sumo_db),
  sumo:create_schema(sumo_test_persons),
  sumo:delete_all(sumo_test_persons),

  sumo:persist(sumo_test_persons, sumo_test_persons:new("Jane", "Doe")),
  sumo:persist(sumo_test_persons, sumo_test_persons:new("John", "Doe", 30)),
  sumo:persist(sumo_test_persons, sumo_test_persons:new("Jane Jr.", "Doe", 5)),
  sumo:persist(sumo_test_persons, sumo_test_persons:new("Joe", "Armstrong")),
  sumo:persist(sumo_test_persons,
               sumo_test_persons:new("Alan", "Turing", 102, "Computer St.")),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backward_compatibility(_Config) ->
  [_, _, _, _, _] = sumo:find_all(sumo_test_persons),

  [_, _, _] = sumo:find_by(sumo_test_persons, [{last_name, "Doe"}]),

  [_] = sumo:find_by(sumo_test_persons, [{name, "Jane"}, {last_name, "Doe"}]).

or_conditional(_Config) ->
  [_, _, _] = sumo:find_by(sumo_test_persons,
                           {'or', [{name, "John"},
                                   {name, "Joe"},
                                   {name, "Alan"}
                                  ]
                           }
                          ),

  [_, _] = sumo:find_by(sumo_test_persons,
                           [{last_name, "Doe"},
                            {'or', [{name, "Jane"},
                                    {name, "Jane Jr."}
                                   ]
                            }
                           ]
                       ).

and_conditional(_Config) ->
  [] = sumo:find_by(sumo_test_persons,
                           {'and', [{name, "John"},
                                    {name, "Joe"},
                                    {name, "Alan"}
                                   ]
                           }
                   ),

  [_, _] = sumo:find_by(sumo_test_persons,
                           {'and', [{last_name, "Doe"},
                                    {'or', [{name, "Jane"},
                                            {name, "Jane Jr."}
                                           ]
                                    }
                                   ]
                           }
                       ).

not_null_conditional(_Config) ->
  [_, _, _] = sumo:find_by(sumo_test_persons, {age, 'not_null'}),

  [_] = sumo:find_by(sumo_test_persons, {address, 'not_null'}).


null_conditional(_Config) ->
  [_, _] = sumo:find_by(sumo_test_persons, {age, 'null'}),

  [_, _, _, _] = sumo:find_by(sumo_test_persons, {address, 'null'}).

operators(_Config) ->
  [_, _] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '<', 100}
                                ]
                        }),

  [_] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '>', 100}
                                ]
                        }),

  [] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '>', 102}
                                ]
                        }),

  [_] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '>=', 102}
                                ]
                        }),


  [_] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '<', 30}
                                ]
                        }),

  [_, _] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '<=', 30}
                                ]
                        }),

  [_, _] = sumo:find_by(sumo_test_persons,
                        {'and', [{age, 'not_null'},
                                 {age, '!=', 30}
                                ]
                        }),

  [_, _, _, _] =
    sumo:find_by(sumo_test_persons, {'and', [{name, 'like', "%J%"}]}),

  [_, _] = sumo:find_by(sumo_test_persons, {'and', [{name, 'like', "%Ja%"}]}).


deeply_nested(_Config) ->
  Conditions = {'or', [{'and', [{age, '>', 100},
                                {address, 'like', "%Ave%"}]},
                       {age, 'null'},
                       {last_name, "Turing"}
                      ]
               },
  [_, _, _] = sumo:find_by(sumo_test_persons, Conditions).
