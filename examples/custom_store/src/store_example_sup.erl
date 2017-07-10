-module(store_example_sup).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noarg).

init(noarg) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 1000
              , period    => 3600
              },

  Children = [],

  {ok, {SupFlags, Children}}.
