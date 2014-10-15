# About
This is an escript that uses sumo_db (and the code in ./src) to implement a blog.

It uses different repositories (mysql, mongo) just as an example and
to make a point about how easy it is to switch or mix different databases.

# Tour
 * Start by taking a rough look at **./run**, and then **src/blog.erl**.
 * Note how:
  * The business logic is handled directly in the "entities" modules
  (blog\_post, blog\_author, etc).
  * The **blog** module is the main entry point to the CRUD routines.
  * Along the code, you only interact very briefly (and simply) with the
  **sumo** module.
  * The impedance between db and entity representations is greatly
  diminished by a clear abstraction level.

# How to run
    ./run
