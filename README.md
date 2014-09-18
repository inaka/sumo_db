sumo_db

# About
This is a work in progress. There's also [an article](http://marcelog.github.com/articles/erlang_persistence_entities.html) about sumo_db.

**sumo_db** aims to ease db access for erlang applications. It offers a very
simple persistance layer capable of interacting with different db's, while
offering a consistent api to your code.

# Overview
 * sumo_db gives you a standard way to define your db schema, no matter the
 db implementation (mongo, mysql, redis, or sqlite3).
 * Your entities encapsulate behavior in code (functions in module) and state
 into ``sumo:doc()``.
 * sumo is the main module. It translates to and from sumo internal records into your
 own state.
 * Each repo is a process *sumo_repo* that calls the actual db driver
 (e.g: sumo_repo_mysql).
 * Some native domain events are supported, that are dispatched through
 a gen_event:notify/2 automatically when an entity is created, updated, deleted.
 Also when a schema is created and when all entities of a given type are
 deleted. Events are described in [this article](http://marcelog.github.com/articles/erlang_epers_persist_entities_domain_events.html)

# About dependencies
In order to avoid having sumo_db require the db drivers (like emysql, emongo, etc),
**you** should include them in your application. Currently, sumo_db has been tested
with the following drivers and versions.

# DB's supported
 * mysql (uses [emysql](https://github.com/Eonblast/Emysql))
 * mongodb (uses [emongo](https://github.com/JacobVorreuter/emongo))

# In progress
 * sqlite3 (uses [erlang-sqlite3](https://github.com/alexeyr/erlang-sqlite3))

# Planned
  * redis
  * mnesia
  * memory

# Example
See: [**examples/blog**](https://github.com/inaka/sumo_db/tree/master/examples/blog)
for a full example. To run it, while being in the top level directory:

    make all blog

# Migrations

There's currently a very basic implementation of DB migrations available. A
`migrations_dir` configuration parameter was added which indicates where sumo
should look for migrations `*.erl` files, it's default value is `src/migrations`.
Migration modules should be named with the following format:
`yyyymmddhhMMss_description_with_undescores`. The most recent migration will be
considered the one with the most recent `year-month-day-hour-min-sec` value.

In order for migrations to work in a given project, sumo_migration should be
added to the docs list in sumo's configuration.

The following targets can be added to the project's Makefile:

```Makefile
migrate:
    erl -pa ebin -pa deps/*/ebin -s your_app -s sumo migrate -config path/to/config

rollback:
    erl -pa ebin -pa deps/*/ebin -s your_app -s sumo rollback -config path/to/config
```

If your migrations folder is not under `src` the you should make sure to compile
all migration modules and include the path to the compiled files in the call
to `erl`.

# TODO
 * Get rid of atoms and use lists.