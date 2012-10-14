# About
This is a work in progress.

**epers** aims to ease db access for erlang applications. It offers a very
simple persistance layer capable of interacting with different db's, while
offering a consistent api to your code.

# Overview
 * epers gives you a standard way to define your db schema, no matter the 
 db implementation (mongo, mysql, redis, etc).
 * Your entities encapsulate behavior in code (functions in module) and state
 into proplists.
 * epers is the main module. It translates to and from #epers_doc into your
 own state.
 * Each repo is a process *epers_repo* that calls the actual db driver
 (e.g: epers_repo_mysql).

# DB's supported
 * mysql (uses [emysql](https://github.com/Eonblast/Emysql))
 * mongodb (uses [emongo](https://github.com/JacobVorreuter/emongo))

# In progress
 * redis (uses [eredis](https://github.com/wooga/eredis))
 * sqlite3 (uses [erlang-sqlite3](https://github.com/alexeyr/erlang-sqlite3))

# Planned
  * mnesia
  * memory

# Example
See: [**examples/blog**](https://github.com/marcelog/epers/tree/master/examples/blog)
for a full example. To run it, while being in the top level directory:

    make all blog

# TODO
 * Get rid of atoms and use lists.
