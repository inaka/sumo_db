-module(person).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).
-export([new/2]).

new(Name, City) ->
  #{name => Name, city => City}.

sumo_schema() ->
  Fields = [
    sumo:new_field(id,   integer, [id, not_null, auto_increment]),
    sumo:new_field(name, string,  [not_null]),
    sumo:new_field(city, string,  [not_null])
  ],
  sumo:new_schema(people, Fields).

sumo_wakeup(Response) -> Response.

sumo_sleep(Person) -> Person.
