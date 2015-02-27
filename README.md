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
please use our public [hipchat room](https://www.hipchat.com/gpBpW3SsT).

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

## Example

See: [**examples/blog**][example-blog] for a full example. To run it, while
being in the top level directory:

    make all blog

## Running Tests

To run tests successfully, you need to follow these steps first:

 * Start the database engines: **MySQL**, **PostgreSQL**, **MongoDB** and
 **ElasticSearch**

 * For **MySQL**, **PostgreSQL** and **MongoDB**, you need to:
    - Create an user (or use defaults) and configure it on `test/test.config`
      file.
    - Create test database `sumo_test` on each DB.

> **Note:**

> - For **MongoDB** you first create the test database and then create an user
    to access that DB. For more information visit [MongoDB Tutorial](http://docs.mongodb.org/manual/tutorial).
> - For **Riak** please follow instruction below first ([<i class="icon-refresh"></i> Riak](#riak)).

## Riak

### Install Riak

To install/upgrade **Riak** please follow the instructions in this link:
[Installing and Upgrading Riak](http://docs.basho.com/riak/latest/ops/building/installing).

### Initial Configurations

Due to **Riak** comes with default configuration, we need to change some
parameters required by `sumo_db`.

**Riak** has a main configuration file `riak.conf`, which you can find into
your installation path `$YOUR_INSTALL_PATH/etc/riak.conf`.

> **Note:** For more information check this link [Configuration Files](http://docs.basho.com/riak/latest/ops/advanced/configs/configuration-files).

First parameter to change is the default **Riak** backend from **Bitcask** to
**LevelDB**. This change also eblables to use [Riak Secondary Indexes](http://docs.basho.com/riak/latest/ops/advanced/configs/secondary-index/).

    storage_backend = leveldb

Then proceed to enable search capabilities:

    search = on

> **Note:** For more information check this link [Riak Search Settings](http://docs.basho.com/riak/latest/ops/advanced/configs/search/).

### Configuring Riak Data Types and Search

First, let's create and activate a bucket type simply called maps that is set up
to store Riak maps:

    $ riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
    $ riak-admin bucket-type activate mapsdmin bucket-type activate maps

Now, let's create a search index called `sumo_test_index` using the default
schema:

    $ curl -XPUT $RIAK_HOST/search/index/sumo_test_index \
        -H 'Content-Type: application/json' \
        -d '{"schema":"_yz_default"}'

With our index created, we can associate our new `sumo_test_index` index with
our `maps` bucket type:

    $ riak-admin bucket-type update maps '{"props":{"search_index":"sumo_test_index"}}'

Now we can start to working with **Riak** from `sumo_db`.

> **Note:** For more information check this link [Riak Data Types and Search](http://docs.basho.com/riak/latest/dev/search/search-data-types/#Maps-Example).

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
