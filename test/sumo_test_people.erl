-module(sumo_test_people).

-behavior(sumo_doc).

%%% sumo_db callbacks
-export([
         sumo_wakeup/1,
         sumo_sleep/1
        ]).

-export([new/2, new/3, new/4, name/1, id/1, age/1]).

-record(person, {id :: integer(),
                 name :: string(),
                 last_name :: string(),
                 age :: integer(),
                 address :: string()}).

-type person() :: #person{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_sleep(person()) -> sumo:doc().
sumo_sleep(Person) ->
    #{id => Person#person.id,
      name => Person#person.name,
      last_name => Person#person.last_name,
      age => Person#person.age,
      address => Person#person.address}.

-spec sumo_wakeup(sumo:doc()) -> person().
sumo_wakeup(Person) ->
    #person{
       id = maps:get(id, Person),
       name = maps:get(name, Person),
       last_name = maps:get(last_name, Person),
       age = from_bin(maps:get(age, Person), integer),
       address = maps:get(address, Person)
      }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Name, LastName) ->
  #person{name = Name,
          last_name = LastName}.

new(Name, LastName, Age) ->
  #person{name = Name,
          last_name = LastName,
          age = Age}.

new(Name, LastName, Age, Address) ->
  #person{name = Name,
          last_name = LastName,
          age = Age,
          address = Address}.

name(Person) ->
  Person#person.name.

id(Person) ->
  Person#person.id.

age(Person) ->
  Person#person.age.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc This helper function is needed by Riak, because data in Riak is stored
%%      as Riak Maps, and values in the them must be binary, so when a data is
%%      stored in Riak, all values in the map are converted to binary, because
%%      of that, is necessary convert values to original types when data is
%%      returned.
from_bin(Bin, integer) when is_binary(Bin) ->
  binary_to_integer(Bin);
from_bin(Bin, float) when is_binary(Bin) ->
  binary_to_float(Bin);
from_bin(Bin, atom) when is_binary(Bin) ->
  binary_to_atom(Bin, utf8);
from_bin(Bin, string) when is_binary(Bin) ->
  binary_to_list(Bin);
from_bin(Bin, _) ->
  Bin.
