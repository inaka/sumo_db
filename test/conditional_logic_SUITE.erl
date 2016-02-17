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
         deeply_nested/1,
         dates/1
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
        sumo:persist(Module, Module:new("Alan", "Turing", 102, "Computer St.")),

        sumo_test_utils:sleep_if_required(Module)
    end,

  lists:foreach(Fun, sumo_test_utils:people_with_conditional_logic()),

  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dates(_Config) ->
  lists:foreach(
    fun do_dates/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_dates(Module) ->
  ct:comment("dates with ~p", [Module]),
  5 = length(sumo:find_all(Module)),

  Now = {Today, _} = calendar:universal_time(),

  0 = length(sumo:find_by(Module, [{birthdate, '>', Today}])),
  5 = length(sumo:find_by(Module, [{birthdate, '==', Today}])),
  5 = length(sumo:find_by(Module, [{birthdate, '=<', Today}])),
  0 = length(sumo:find_by(Module, [{created_at, '>', Now}])),
  5 = length(sumo:find_by(Module, [{created_at, '=<', Now}])).

backward_compatibility(_Config) ->
  lists:foreach(
    fun do_backward_compatibility/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_backward_compatibility(Module) ->
  ct:comment("backward_compatibility with ~p", [Module]),
  5 = length(sumo:find_all(Module)),
  3 = length(sumo:find_by(Module, [{last_name, "Doe"}])),
  1 = length(sumo:find_by(Module, [{name, "Jane"}, {last_name, "Doe"}])).

or_conditional(_Config) ->
  lists:foreach(
    fun do_or_conditional/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_or_conditional(Module) ->
  ct:comment("or_conditional with ~p", [Module]),
  3 = length(sumo:find_by(Module,
                          {'or', [{name, "John"},
                                  {name, "Joe"},
                                  {name, "Alan"}
                                 ]
                          })),

  2 = length(sumo:find_by(Module,
                          [{last_name, "Doe"},
                           {'or', [{name, "Jane"},
                                   {name, "Jane Jr."}
                                  ]
                           }])).

and_conditional(_Config) ->
  lists:foreach(
    fun do_and_conditional/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_and_conditional(Module) ->
  ct:comment("and_conditional with ~p", [Module]),
  0 = length(sumo:find_by(Module,
                          {'and', [{name, "John"},
                                   {name, "Joe"},
                                   {name, "Alan"}
                                  ]
                          })),

  2 = length(sumo:find_by(Module,
                          {'and', [{last_name, "Doe"},
                                   {'or', [{name, "Jane"},
                                           {name, "Jane Jr."}
                                          ]
                                   }
                                  ]
                          })).

not_null_conditional(_Config) ->
  lists:foreach(
    fun do_not_null_conditional/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_not_null_conditional(Module) ->
  ct:comment("not_null_conditional with ~p", [Module]),
  5 = length(sumo:find_by(Module, {age, 'not_null'})),
  5 = length(sumo:find_by(Module, {address, 'not_null'})).

null_conditional(_Config) ->
  lists:foreach(
    fun do_null_conditional/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_null_conditional(Module) ->
  ct:comment("null_conditional with ~p", [Module]),
  0 = length(sumo:find_by(Module, {age, 'null'})),
  0 = length(sumo:find_by(Module, {address, 'null'})).

operators(_Config) ->
  lists:foreach(
    fun do_operators/1,
    sumo_test_utils:people_with_numeric_sort()),
  {comment, ""}.

do_operators(Module) ->
  ct:comment("operators with ~p", [Module]),
  4 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '<', 100}
                                  ]
                          })),

  1 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '>', 100}
                                  ]
                          })),

  0 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '>', 102}
                                  ]
                          })),

  1 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '>=', 102}
                                  ]
                          })),


  3 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '<', 30}
                                  ]
                          })),

  4 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '=<', 30}
                                  ]
                          })),

  1 = length(sumo:find_by(Module,
                          {'or', [{age, 'null'},
                                  {age, '==', 30}
                                 ]
                          })),

  4 = length(sumo:find_by(Module,
                          {'and', [{age, 'not_null'},
                                   {age, '/=', 30}
                                  ]
                          })),

  case lists:member(Module, sumo_test_utils:people_with_like()) of
    true ->
      4 = length(sumo:find_by(Module, {name, 'like', "J%"})),
      2 = length(sumo:find_by(Module, {'and', [{name, 'like', "Ja%"}]})),
      1 = length(sumo:find_by(Module, {name, 'like', "A%"})),
      2 = length(sumo:find_by(Module, {name, 'like', "%n"}));
    false ->
      ok
  end.

deeply_nested(_Config) ->
  lists:foreach(
    fun do_deeply_nested/1,
    sumo_test_utils:people_with_conditional_logic()),
  {comment, ""}.

do_deeply_nested(Module) ->
  ct:comment("deeply_nested with ~p", [Module]),
  Conditions = {'or', [{'and', [{age, '>', 100},
                                {address, '==', "something"}]},
                       {age, 'null'},
                       {last_name, "Turing"}
                      ]
               },
  1 = length(sumo:find_by(Module, Conditions)).
