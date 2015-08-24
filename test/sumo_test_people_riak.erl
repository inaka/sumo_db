-module(sumo_test_people_riak).

%%% sumo_db callbacks
-export([ sumo_schema/0
        , sumo_wakeup/1
        , sumo_sleep/1
        ]).

-export([ new/2
        , new/3
        , new/4
        , name/1
        , id/1
        , age/1
        , date/1
        , datetime/1
        ]).

-record(person, {id        :: integer(),
                 name      :: string(),
                 last_name :: string(),
                 age       :: integer(),
                 address   :: string(),
                 date      :: calendar:date(),
                 datetime  :: calendar:datetime()}).

-type person() :: #person{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,        string, [id, {length, 255}, not_null]),
     sumo:new_field(name,      string, [{length, 255}, not_null]),
     sumo:new_field(last_name, string, [{length, 255}, not_null]),
     sumo:new_field(age,       integer),
     sumo:new_field(address,   string, [{length, 255}]),
     sumo:new_field(date,      date),
     sumo:new_field(datetime,  datetime)
    ],
  sumo:new_schema(?MODULE, Fields).

-spec sumo_sleep(person()) -> sumo:doc().
sumo_sleep(Person) ->
  #{id        => Person#person.id,
    name      => Person#person.name,
    last_name => Person#person.last_name,
    age       => Person#person.age,
    address   => Person#person.address,
    date      => Person#person.date,
    datetime  => Person#person.datetime}.

-spec sumo_wakeup(sumo:doc()) -> person().
sumo_wakeup(Person) ->
  #person{
    id        = maps:get(id, Person),
    name      = maps:get(name, Person),
    last_name = maps:get(last_name, Person),
    age       = maps:get(age, Person),
    address   = maps:get(address, Person),
    date      = maps:get(date, Person),
    datetime  = maps:get(datetime, Person)
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Name, LastName) ->
  new(Name, LastName, undefined).

new(Name, LastName, Age) ->
  new(Name, LastName, Age, undefined).

new(Name, LastName, Age, Address) ->
  Datetime = {Date, _} = calendar:universal_time(),
  #person{name = Name,
          last_name = LastName,
          age = Age,
          address = Address,
          date = Date,
          datetime = Datetime}.

name(Person) ->
  Person#person.name.

id(Person) ->
  Person#person.id.

age(Person) ->
  Person#person.age.

date(Person) ->
  Person#person.date.

datetime(Person) ->
  Person#person.datetime.
