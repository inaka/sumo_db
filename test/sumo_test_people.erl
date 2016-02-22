-module(sumo_test_people).

-behavior(sumo_doc).

%%% sumo_db callbacks
-export([
  sumo_wakeup/1,
  sumo_sleep/1
]).

%% API
-export([
  new/2,
  new/3,
  new/4,
  new/5,
  new/6,
  new/7,
  new/8,
  name/1,
  last_name/1,
  id/1,
  age/1,
  address/1,
  birthdate/1,
  created_at/1,
  height/1,
  description/1,
  profile_image/1
]).

-record(person, {
  id            :: integer() | binary(),
  name          :: string(),
  last_name     :: string(),
  age           :: integer(),
  address       :: string(),
  birthdate     :: calendar:date(),
  created_at    :: calendar:datetime(),
  height        :: float(),
  description   :: binary(),
  profile_image :: binary()
}).

-type person() :: #person{}.

%%%=============================================================================
%%% sumo_doc callbacks
%%%=============================================================================

-spec sumo_sleep(person()) -> sumo:doc().
sumo_sleep(Person) ->
  #{id            => Person#person.id,
    name          => Person#person.name,
    last_name     => Person#person.last_name,
    age           => Person#person.age,
    address       => Person#person.address,
    birthdate     => Person#person.birthdate,
    created_at    => Person#person.created_at,
    height        => Person#person.height,
    description   => Person#person.description,
    profile_image => Person#person.profile_image}.

-spec sumo_wakeup(sumo:doc()) -> person().
sumo_wakeup(Person) ->
  #person{
    id            = maps:get(id, Person),
    name          = maps:get(name, Person),
    last_name     = maps:get(last_name, Person),
    age           = maps:get(age, Person),
    address       = maps:get(address, Person),
    birthdate     = maps:get(birthdate, Person),
    created_at    = maps:get(created_at, Person),
    height        = maps:get(height, Person),
    description   = maps:get(description, Person),
    profile_image = maps:get(profile_image, Person)
  }.

%%%=============================================================================
%%% API
%%%=============================================================================

new(Name, LastName) ->
  new(Name, LastName, 0).

new(Name, LastName, Age) ->
  new(Name, LastName, Age, "").

new(Name, LastName, Age, Address) ->
  {BirthDate, _} = calendar:universal_time(),
  new(Name, LastName, Age, Address, BirthDate).

new(Name, LastName, Age, Address, BirthDate) ->
  new(Name, LastName, Age, Address, BirthDate, 0.0).
new(Name, LastName, Age, Address, BirthDate, Height) ->
  new(Name, LastName, Age, Address, BirthDate, Height, "").

new(Name, LastName, Age, Address, BirthDate, Height, Description) ->
  new(Name, LastName, Age, Address, BirthDate, Height, Description, <<>>).

new(Name,
    LastName,
    Age,
    Address,
    BirthDate,
    Height,
    Description,
    ProfileImage) ->
  Datetime = calendar:universal_time(),
  #person{
    name          = Name,
    last_name     = LastName,
    age           = Age,
    address       = Address,
    birthdate     = BirthDate,
    created_at    = Datetime,
    height        = Height,
    description   = Description,
    profile_image = ProfileImage}.

name(Person) ->
  Person#person.name.

last_name(Person) ->
  Person#person.last_name.

id(Person) ->
  Person#person.id.

age(Person) ->
  Person#person.age.

address(Person) ->
  Person#person.address.

birthdate(Person) ->
  Person#person.birthdate.

created_at(Person) ->
  Person#person.created_at.

height(Person) ->
  Person#person.height.

description(Person) ->
  Person#person.description.

profile_image(Person) ->
  Person#person.profile_image.
