# Change Log

All notable changes to this project will be documented in this file.

## `master`

### Added

 - None.

### Deprecated

 - [#62] Renamed all `sumo_repo*` modules to `sumo_store*`. Existing repo modules in your application should be updated with the new names.

### Removed

 - None.

### Fixed

 - None.

## 0.1.4 - 2014-11-04

### Added

 - [#21] asc/desc option for ordering.
 - `CONTRIBUTORS.md`.
 - [#3]  Full conditional logic for MySQL and MongoDB.
 - [#61] Add `erlang.mk`.
 - [#67] GitHub page http://inaka.github.io/sumo_db/.
 - [#74] ElasticSearch backend and repo.
 - [#77] Defaults for worker pool's overrun configuration values.
 - [#51] Use a proper overrun warning for wpool.

### Deprecated

 - None.

### Removed

 - [#87] Removed SQLite repo and dependency.

### Fixed

 - [#70] Log execution time in miliseconds.
 - [#72] Provide version for dependencies.
 - [#79] Remove git account for dependencies.
 - [#81] Blog example doesn't build cleanly.
 - [#80] Fixed the run escript to not throw errors for make blog.

## 0.1.3 - 2014-09-23

### Added

 - [#68] Accept `wpool_opts` entry in configuration file.

### Deprecated

 - None.

### Removed

 - None.

### Fixed

 - None.

## 0.1.2 - 2014-07-11

### Added

 - [#58] Upgraded rebar to 2.5.0 so that it plays nicely with erlang.mk.

### Deprecated

 - None.

### Removed

 - None.

### Fixed

 - None.

## 0.1.1 - 2014-07-02

### Added

 - [#56] Ready for R17. Fixed all compilation warnings.

### Deprecated

 - None.

### Removed

 - None.

### Fixed

 - None.

## 0.1.0 - 2014-06-04

### Added

- All basic sumo_db features:
  - Backends
  - Repos
  - Events

### Deprecated

 - None.

### Removed

 - None.

### Fixed

 - None.
