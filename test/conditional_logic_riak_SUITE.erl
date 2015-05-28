-module(conditional_logic_riak_SUITE).

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
  [F || {F, 1} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  sumo_test_utils:start_apps(),

  Module = sumo_test_people_riak,

  sumo:create_schema(Module),
  sumo:delete_all(Module),

  sumo:persist(Module, Module:new("Jane", "Doe")),
  sumo:persist(Module, Module:new("John", "Doe", 30, "2015-01-01")),
  sumo:persist(Module, Module:new("Jane Jr.", "Doe", 5, "2015-01-05")),
  sumo:persist(Module, Module:new("Joe", "Armstrong")),
  sumo:persist(Module, Module:new("Alan", "Turing", 102, "2015-01-10")),

  timer:sleep(5000),

  [{module, Module} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

backward_compatibility(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [_, _, _, _, _] = sumo:find_all(Module),

  [_, _, _] = sumo:find_by(Module, [{last_name, "Doe"}]),

  [_] = sumo:find_by(Module, [{name, "Jane"}, {last_name, "Doe"}]).

or_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

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

and_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

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

not_null_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [_, _, _] = sumo:find_by(Module, {age, 'not_null'}),

  [_, _, _] = sumo:find_by(Module, {address, 'not_null'}).


null_conditional(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [_, _] = sumo:find_by(Module, {age, 'null'}),

  [_, _] = sumo:find_by(Module, {address, 'null'}).

operators(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  [_] = sumo:find_by(Module,
                        {'and', [{address, 'not_null'},
                                 {address, '<', "2015-01-05"}
                                ]
                        }),

  [_] = sumo:find_by(Module,
                     {'and', [{address, 'not_null'},
                              {address, '>', "2015-01-05"}
                             ]
                     }),

  [] = sumo:find_by(Module,
                    {'and', [{address, 'not_null'},
                             {address, '>', "2015-01-10"}
                            ]
                    }),

  [_] = sumo:find_by(Module,
                     {'and', [{address, 'not_null'},
                              {address, '>=', "2015-01-10"}
                             ]
                     }),


  [] = sumo:find_by(Module,
                     {'and', [{address, 'not_null'},
                              {address, '<', "2015-01-01"}
                             ]
                     }),

  [_] = sumo:find_by(Module,
                        {'and', [{address, 'not_null'},
                                 {address, '=<', "2015-01-01"}
                                ]
                        }),

  [_, _, _] = sumo:find_by(Module,
                           {'or', [{address, 'null'},
                                   {age, '==', 30}
                                  ]
                           }),

  [_, _] = sumo:find_by(Module,
                        {'and', [{address, 'not_null'},
                                 {age, '/=', 30}
                                ]
                        }),

  case lists:member(Module, sumo_test_utils:people_with_like()) of
    true ->
      [_, _, _, _] = sumo:find_by(Module, {name, 'like', "J%"}),

      [_, _] = sumo:find_by(Module, {'and', [{name, 'like', "Ja%"}]}),

      [_] = sumo:find_by(Module, {name, 'like', "A%"}),

      [_, _] = sumo:find_by(Module, {name, 'like', "%n"});
    false ->
      ok
  end.

deeply_nested(Config) ->
  {_, Module} = lists:keyfind(module, 1, Config),

  Conditions = {'or', [{'and', [{age, '>', 100},
                                {address, '==', "something"}]},
                       {age, 'null'},
                       {last_name, "Turing"}
                      ]
               },
  [_, _, _] = sumo:find_by(Module, Conditions).
