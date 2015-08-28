-module(sumo_test_people).

-behavior(sumo_doc).

%%% sumo_db callbacks
-export([
         sumo_wakeup/1,
         sumo_sleep/1
        ]).

-export([new/2, new/3, new/4, name/1, id/1, age/1, birthdate/1, created_at/1]).

-record(person, {id :: integer(),
                 name :: string(),
                 last_name :: string(),
                 age :: integer(),
                 address :: string(),
                 birthdate  :: calendar:date(),
                 created_at :: calendar:datetime()}).

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
    address => Person#person.address,
    birthdate  => Person#person.birthdate,
    created_at => Person#person.created_at}.

-spec sumo_wakeup(sumo:doc()) -> person().
sumo_wakeup(Person) ->
  #person{
    id = maps:get(id, Person),
    name = maps:get(name, Person),
    last_name = maps:get(last_name, Person),
    age = maps:get(age, Person),
    address = maps:get(address, Person),
    birthdate  = maps:get(birthdate, Person),
    created_at = maps:get(created_at, Person)
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Name, LastName) -> new(Name, LastName, undefined).

new(Name, LastName, Age) -> new(Name, LastName, Age, undefined).

new(Name, LastName, Age, Address) ->
  Datetime = {Date, _} = calendar:universal_time(),
  #person{name = Name,
          last_name   = LastName,
          age         = Age,
          address     = Address,
          birthdate   = Date,
          created_at  = Datetime}.

name(Person) ->
  Person#person.name.

id(Person) ->
  Person#person.id.

age(Person) ->
  Person#person.age.

birthdate(Person) ->
  Person#person.birthdate.

created_at(Person) ->
  Person#person.created_at.
