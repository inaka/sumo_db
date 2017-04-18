-module(sumo_test_people).

%%% sumo_db callbacks
-export([
  sumo_schema/0,
  sumo_wakeup/1,
  sumo_sleep/1
]).

%% API
-export([
  new/2,
  new/3,
  new/4,
  name/1,
  from_map/1,
  last_name/1,
  id/1,
  age/1,
  address/1,
  birthdate/1,
  created_at/1,
  height/1,
  description/1,
  profile_image/1,
  is_blocked/1,
  status/1,
  weird_field1/1,
  weird_field2/1,
  weird_field3/1
]).

-type id()            :: integer() | binary() | undefined.
-type name()          :: binary() | undefined.
-type last_name()     :: binary() | undefined.
-type age()           :: integer() | undefined.
-type address()       :: binary() | undefined.
-type birthdate()     :: calendar:date() | undefined.
-type created_at()    :: calendar:datetime() | undefined.
-type height()        :: float() | undefined.
-type description()   :: binary() | undefined.
-type profile_image() :: binary() | undefined.
-type status()        :: binary() | undefined.
-type weird_field()   :: term() | undefined.

-record(person, {
  id            :: id(),
  name          :: name(),
  last_name     :: last_name(),
  age           :: age(),
  address       :: address(),
  birthdate     :: birthdate(),
  created_at    :: created_at(),
  height        :: height(),
  description   :: description(),
  profile_image :: profile_image(),
  is_blocked    :: boolean(),
  status        :: status(),
  weird_field1  :: weird_field(),
  weird_field2  :: weird_field(),
  weird_field3  :: weird_field(),
  missing       :: any()
}).

-type person() :: #person{}.

%%%=============================================================================
%%% sumo_doc callbacks
%%%=============================================================================

-spec sumo_schema() -> no_return().
sumo_schema() -> exit(should_be_implemented_by_children).

-spec sumo_sleep(Person :: person()) -> sumo:model().
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
    profile_image => Person#person.profile_image,
    is_blocked    => Person#person.is_blocked,
    status        => Person#person.status,
    weird_field1  => Person#person.weird_field1,
    weird_field2  => Person#person.weird_field2,
    weird_field3  => Person#person.weird_field3,
    missing       => Person#person.missing}.

-spec sumo_wakeup(Person :: sumo:model()) -> person().
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
    profile_image = maps:get(profile_image, Person),
    is_blocked    = maps:get(is_blocked, Person),
    status        = maps:get(status, Person),
    weird_field1  = maps:get(weird_field1, Person),
    weird_field2  = maps:get(weird_field2, Person),
    weird_field3  = maps:get(weird_field3, Person),
    missing       = maps:get(missing, Person, undefined)
  }.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new(Name, LastName) -> Person when
  Name     :: name(),
  LastName :: last_name(),
  Person   :: person().
new(Name, LastName) ->
  new(Name, LastName, undefined).

-spec new(Name, LastName, Age) -> Person when
  Name     :: name(),
  LastName :: last_name(),
  Age      :: age(),
  Person   :: person().
new(Name, LastName, Age) ->
  new(Name, LastName, Age, undefined).

-spec new(Name, LastName, Age, Address) -> Person when
  Name     :: name(),
  LastName :: last_name(),
  Age      :: age(),
  Address  :: address(),
  Person   :: person().
new(Name, LastName, Age, Address) ->
  {BirthDate, _} = calendar:universal_time(),
  Datetime = calendar:universal_time(),
  from_map(#{
    name       => Name,
    last_name  => LastName,
    age        => Age,
    address    => Address,
    birthdate  => BirthDate,
    created_at => Datetime
  }).

-spec from_map(map()) -> person().
from_map(Map) ->
  #person{
    name          = maps:get(name, Map, undefined),
    last_name     = maps:get(last_name, Map, undefined),
    age           = maps:get(age, Map, undefined),
    address       = maps:get(address, Map, undefined),
    birthdate     = maps:get(birthdate, Map, undefined),
    created_at    = maps:get(created_at, Map, undefined),
    height        = maps:get(height, Map, undefined),
    description   = maps:get(description, Map, undefined),
    profile_image = maps:get(profile_image, Map, undefined),
    is_blocked    = maps:get(is_blocked, Map, false),
    weird_field1  = maps:get(weird_field1, Map, undefined),
    weird_field2  = maps:get(weird_field2, Map, undefined),
    weird_field3  = maps:get(weird_field3, Map, undefined),
    missing       = maps:get(missing, Map, undefined)
  }.

-spec name(Person :: person()) -> name().
name(Person) ->
  Person#person.name.

-spec last_name(Person :: person()) -> last_name().
last_name(Person) ->
  Person#person.last_name.

-spec id(Person :: person()) -> id().
id(Person) ->
  Person#person.id.

-spec age(Person :: person()) -> age().
age(Person) ->
  Person#person.age.

-spec address(Person :: person()) -> address().
address(Person) ->
  Person#person.address.

-spec birthdate(Person :: person()) -> birthdate().
birthdate(Person) ->
  Person#person.birthdate.

-spec created_at(Person :: person()) -> created_at().
created_at(Person) ->
  Person#person.created_at.

-spec height(Person :: person()) -> height().
height(Person) ->
  Person#person.height.

-spec description(Person :: person()) -> description().
description(Person) ->
  Person#person.description.

-spec profile_image(Person :: person()) -> profile_image().
profile_image(Person) ->
  Person#person.profile_image.

-spec is_blocked(Person :: person()) -> boolean().
is_blocked(Person) ->
  Person#person.is_blocked.

-spec status(Person :: person()) -> status().
status(Person) ->
  Person#person.status.

-spec weird_field1(Person :: person()) -> weird_field().
weird_field1(Person) ->
  Person#person.weird_field1.

-spec weird_field2(Person :: person()) -> weird_field().
weird_field2(Person) ->
  Person#person.weird_field2.

-spec weird_field3(Person :: person()) -> weird_field().
weird_field3(Person) ->
  Person#person.weird_field3.
