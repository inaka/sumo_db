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
  new/5,
  new/6,
  new/7,
  new/8,
  new/9,
  name/1,
  last_name/1,
  id/1,
  age/1,
  address/1,
  birthdate/1,
  created_at/1,
  height/1,
  description/1,
  profile_image/1,
  weird_field/1
]).

-type id()            :: integer() | binary() | undefined.
-type name()          :: binary().
-type last_name()     :: binary().
-type age()           :: integer() | undefined.
-type address()       :: binary() | undefined.
-type birthdate()     :: calendar:date() | undefined.
-type created_at()    :: calendar:datetime().
-type height()        :: float() | undefined.
-type description()   :: binary() | undefined.
-type profile_image() :: binary() | undefined.
-type weird_field()   :: term().

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
  weird_field   :: weird_field()
}).

-type person() :: #person{}.

%%%=============================================================================
%%% sumo_doc callbacks
%%%=============================================================================

-spec sumo_schema() -> no_return().
sumo_schema() -> throw(should_be_implemented_by_children).

-spec sumo_sleep(Person :: person()) -> sumo:doc().
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
    weird_field   => Person#person.weird_field}.

-spec sumo_wakeup(Person :: sumo:doc()) -> person().
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
    weird_field   = maps:get(weird_field, Person)
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
  new(Name, LastName, Age, Address, BirthDate).

-spec new(Name, LastName, Age, Address, BirthDate) -> Person when
  Name      :: name(),
  LastName  :: last_name(),
  Age       :: age(),
  Address   :: address(),
  BirthDate :: birthdate(),
  Person    :: person().
new(Name, LastName, Age, Address, BirthDate) ->
  new(Name, LastName, Age, Address, BirthDate, undefined).

-spec new(Name, LastName, Age, Address, BirthDate, Height) -> Person when
  Name      :: name(),
  LastName  :: last_name(),
  Age       :: age(),
  Address   :: address(),
  BirthDate :: birthdate(),
  Height    :: height(),
  Person    :: person().
new(Name, LastName, Age, Address, BirthDate, Height) ->
  new(Name, LastName, Age, Address, BirthDate, Height, undefined).

-spec new(
  Name, LastName, Age, Address, BirthDate, Height, Description
) -> Person when
  Name        :: name(),
  LastName    :: last_name(),
  Age         :: age(),
  Address     :: address(),
  BirthDate   :: birthdate(),
  Height      :: height(),
  Description :: description(),
  Person      :: person().
new(Name, LastName, Age, Address, BirthDate, Height, Description) ->
  new(Name, LastName, Age, Address, BirthDate, Height, Description, undefined).

-spec new(
  Name, LastName, Age, Address, BirthDate, Height, Description, ProfileImage
) -> Person when
  Name         :: name(),
  LastName     :: last_name(),
  Age          :: age(),
  Address      :: address(),
  BirthDate    :: birthdate(),
  Height       :: height(),
  Description  :: description(),
  ProfileImage :: profile_image(),
  Person       :: person().
new(Name, LastName, Age, Address, BirthDate,
    Height, Description, ProfileImage) ->
  new(
    Name, LastName, Age, Address, BirthDate, Height,
    Description, ProfileImage, undefined).

-spec new(
  Name, LastName, Age, Address, BirthDate, Height,
  Description, ProfileImage, WeirdField
) -> Person when
  Name         :: name(),
  LastName     :: last_name(),
  Age          :: age(),
  Address      :: address(),
  BirthDate    :: birthdate(),
  Height       :: height(),
  Description  :: description(),
  ProfileImage :: profile_image(),
  WeirdField   :: weird_field(),
  Person       :: person().
new(Name,
    LastName,
    Age,
    Address,
    BirthDate,
    Height,
    Description,
    ProfileImage,
    WeirdField) ->
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
    profile_image = ProfileImage,
    weird_field   = WeirdField
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

-spec weird_field(Person :: person()) -> weird_field().
weird_field(Person) ->
  Person#person.weird_field.
