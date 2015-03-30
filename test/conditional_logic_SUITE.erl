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

  lists:foreach(Fun, sumo_test_utils:people_with_conditional_logic()),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
backward_compatibility(_Config) ->
  lists:foreach(
    fun do_backward_compatibility/1,
    sumo_test_utils:people_with_conditional_logic()).

do_backward_compatibility(Module) ->
  [_, _, _, _, _] = sumo:find_all(Module),

  [_, _, _] = sumo:find_by(Module, [{last_name, "Doe"}]),

  [_] = sumo:find_by(Module, [{name, "Jane"}, {last_name, "Doe"}]).

or_conditional(_Config) ->
  lists:foreach(
    fun do_or_conditional/1,
    sumo_test_utils:people_with_conditional_logic()).

do_or_conditional(Module) ->
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

and_conditional(_Config) ->
  lists:foreach(
    fun do_and_conditional/1,
    sumo_test_utils:people_with_conditional_logic()).

do_and_conditional(Module) ->
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

not_null_conditional(_Config) ->
  lists:foreach(
    fun do_not_null_conditional/1,
    sumo_test_utils:people_with_conditional_logic()).

do_not_null_conditional(Module) ->
  [_, _, _] = sumo:find_by(Module, {age, 'not_null'}),

  [_] = sumo:find_by(Module, {address, 'not_null'}).


null_conditional(_Config) ->
  lists:foreach(
    fun do_null_conditional/1,
    sumo_test_utils:people_with_conditional_logic()).

do_null_conditional(Module) ->
  [_, _] = sumo:find_by(Module, {age, 'null'}),

  [_, _, _, _] = sumo:find_by(Module, {address, 'null'}).

operators(_Config) ->
  lists:foreach(
    fun do_operators/1,
    sumo_test_utils:people_with_conditional_logic()).

do_operators(Module) ->
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

  case lists:member(Module, sumo_test_utils:people_with_like()) of
    true ->
      [_, _, _, _] = sumo:find_by(Module, {name, 'like', "J%"}),

      [_, _] = sumo:find_by(Module, {'and', [{name, 'like', "Ja%"}]}),

      [_] = sumo:find_by(Module, {name, 'like', "A%"}),

      [_, _] = sumo:find_by(Module, {name, 'like', "%n"});
    false ->
      ok
  end.

deeply_nested(_Config) ->
  lists:foreach(
    fun do_deeply_nested/1,
    sumo_test_utils:people_with_conditional_logic()).

do_deeply_nested(Module) ->
  Conditions = {'or', [{'and', [{age, '>', 100},
                                {address, '==', "something"}]},
                       {age, 'null'},
                       {last_name, "Turing"}
                      ]
               },
  [_, _, _] = sumo:find_by(Module, Conditions).
