[![Stories in Ready](https://badge.waffle.io/inaka/sumo_db.png?label=ready&title=Ready)](https://waffle.io/inaka/sumo_db)
# sumo_db


## About

This is a work in progress. There's also [an article][sumo-article] about
sumo_db. This articles might be a little outdated by now, but can still
provide some basic information on how to get started.

**sumo_db** aims to ease db access for erlang applications. It offers a very
simple persistance layer capable of interacting with different db's, while
offering a consistent api to your code.


## Contact Us

For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue][issue] in this repo (or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)


## Overview

 * sumo_db gives you a standard way to define your db schema, regardless of the
 db implementation (mongo, mysql, redis, elasticsearch, etc.).

 * Your entities encapsulate behavior in code (i.e. functions in a module) and
 state in a ``sumo:doc()`` implementation.

 * `sumo` is the main module. It translates to and from sumo internal records
 into your own state.

 * Each repo is managed by a worker pool of processes, each one using a module
 that implements *sumo_repo* and calls the actual db driver
 (e.g: sumo_repo_mysql).

 * Some native domain events are supported, that are dispatched through a
 `gen_event:notify/2` automatically when an entity is created, updated, deleted.
 Also when a schema is created and when all entities of a given type are
 deleted. Events are described in [this article][domain-article].

 * Full conditional logic support when using `find_by/2` and `delete_by/2`
 function. You can find more information about the syntax of this conditional
 logic operators [here][cond-syntax].

 * Support for sorting (`asc` or `desc`) based on multiple fields unsing
 `find_by/5` and `find_all/4` functions. For example this
 `[{age, desc}, {name, asc}]]` will sort descendently by `age` and ascendently
  by `name`.


## Backends, Stores and Repositories modules

These three concepts have a specific meaning in the context of sumo_db.

 - **Backend**: holds the connection to a single instance of a database, which
 can be MySql, MongoDB, ElasticSearch or any other that's implemented.

 - **Store**: implements the specific operations that modify the contents of the
 backend and retrieves the information it holds.

 - **Repository**: the application that uses `sumo_db` should implement one
 repository for each entity that's defined in it. The repository is the module
 that bridges the model and the store.


## Implementing an Adapter

TO implement an adapter, you must implement two behaviors: `sumo_backend` and
`sumo_store`.

## Events

Sumo dispatches events when things happen. An Event has this structure:
```erlang
{EventId, Model, Event, Args}
```
- `EventId` is a `reference()` which identifies the event
- `Model` is the model where the event happend, for example, if we are creating a new entitiy in the model `people` the value of `Model` would be `people`.
- `Event` is the type of the event.
- `Args` extra data sent.

Supported types of events:

- `pre_persisted` just before persisting some entity. This event has the entity we want to persist as `Args`. It is dispatched on this function:
    - `sumo:persist/2`
- `persisted` just after persisting some entity. This event has the persisted entity as `Args`. This Event has the same `EventId` as its `pre_persisted` event. It is dispatched on this function:
    - `sumo:persist/2`
- `pre_delete_all` just before deleting all entities for a model. This event has no `Args`. It is dispatched on this function:
    - `sumo:delete_all/1`
- `deleted_all` just after deleting all entities for a model. This event has no `Args`. This Event has the same `EventId` as its `pre_delete_all` event. It is dispatched on this function:
    - `sumo:delete_all/1`
- `pre_deleted` just before deleting an entity. This event has the entity id as `Args`. It is dispatched on this function:
    - `sumo:delete/2`
- `deleted` just after deleting an entity. This event has the entity id as `Args`. This Event has the same `EventId` as its `pre_deleted` event. It is dispatched on this function:
    - `sumo:delete/2`
- `pre_deleted_total` just before deleting by some delete conditions. This event has the sumo conditions as `Args`. It is dispatched on this function:
    - `sumo:delete_by/2`
- `deleted_total` just after deleting by some delete conditions. This event has a list with the number of entities deleted and the delete conditions as `Args`. This Event has the same `EventId` as its `pre_deleted_total` event. It is dispatched on this function:
    - `sumo:delete_by/2`
- `pre_schema_created` just before creating a sumo schema. This event has no `Args`. It is dispatched on this function:
    - `sumo:create_schema/2`
- `schema_created` just after creating a sumo schema. This event has no `Args`. This Event has the same `EventId` as its `pre_schema_created` event. It is dispatched on this function:
    - `sumo:create_schema/2`

Sumo requires users to add their own `gen_event`'s in order to handle those events. In order to add them Users have to configure sumo properly. In the `config` file we can add them like this under `sumo_db` configuration:
```erlang
{events, [
     {'_', sumo_test_people_events_manager},
     {people, sumo_test_people_events_manager}
   ]}
```
Sumo allows us to add a `gen_event` to one type of model (i.e. `people`) or for all (`'_'`).

## Example

See: [**examples/blog**][example-blog] for a full example. To run it, while
being in the top level directory:

    make all blog


## Running Dialyzer

```
$ rebar3 dialyzer
```


## Running Tests

```
$ rebar3 ct
```

## Change Log

All notable changes to this project will be documented in the
[CHANGELOG.md](CHANGELOG.md).


## Contributors

We want to thank all of [our contributors](CONTRIBUTORS.md) for their hard work
:muscle:.

 [sumo-article]: http://marcelog.github.com/articles/erlang_persistence_entities.html
 [domain-article]: http://marcelog.github.com/articles/erlang_epers_persist_entities_domain_events.html
 [issue]: https://github.com/inaka/sumo_db/issues/new
 [example-blog]: https://github.com/inaka/sumo_db/tree/master/examples/blog
 [cond-syntax]: https://github.com/inaka/sumo_db/wiki/Conditional-Logic-Syntax
