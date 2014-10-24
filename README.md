# sumo_db

## About
This is a work in progress. There's also [an article](http://marcelog.github.com/articles/erlang_persistence_entities.html) about sumo_db.

**sumo_db** aims to ease db access for erlang applications. It offers a very
simple persistance layer capable of interacting with different db's, while
offering a consistent api to your code.

## Contact Us
For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](https://www.hipchat.com/gpBpW3SsT).
e
If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/sumo_db/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## Overview
 * sumo_db gives you a standard way to define your db schema, regardless of the db implementation (mongo, mysql, redis, elasticsearch, etc.).
 * Your entities encapsulate behavior in code (functions in module) and state into ``sumo:doc()``.
 * sumo is the main module. It translates to and from sumo internal records into your own state.
 * Each repo is managed by a worker pool of processes, each one using a module that implements *sumo_repo* and calls the actual db driver (e.g: sumo_repo_mysql).
 * Some native domain events are supported, that are dispatched through a `gen_event:notify/2` automatically when an entity is created, updated, deleted.
 Also when a schema is created and when all entities of a given type are deleted. Events are described in [this article](http://marcelog.github.com/articles/erlang_epers_persist_entities_domain_events.html)

## Example
See: [**examples/blog**](https://github.com/inaka/sumo_db/tree/master/examples/blog)
for a full example. To run it, while being in the top level directory:

    make all blog

## Contributors
We want to thank all of [our contributors](CONTRIBUTORS.md) for their hard work :muscle:
