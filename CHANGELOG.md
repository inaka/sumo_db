# Change Log

## [0.3.7](https://github.com/inaka/sumo_db/tree/HEAD)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.6...HEAD)

**Fixed bugs:**

- UPDATE statement for PostgreSQL is missing commas [\#150](https://github.com/inaka/sumo_db/issues/150)

**Closed issues:**

- Update rebar.config file  [\#157](https://github.com/inaka/sumo_db/issues/157)

- Update Gun dependency to 0.1.9 [\#155](https://github.com/inaka/sumo_db/issues/155)

- Please update tirerl [\#154](https://github.com/inaka/sumo_db/issues/154)

- Fix 'sumo\_riak\_store' to export utility functions which are currently private [\#140](https://github.com/inaka/sumo_db/issues/140)

**Merged pull requests:**

- \[Fix \#157\] Update tirerl 0.1.6 version in makefile and rebar.config file [\#158](https://github.com/inaka/sumo_db/pull/158) ([davecaos](https://github.com/davecaos))

- \[Fix \#154\] Update Tirerl dependnecy to 0.1.5 [\#156](https://github.com/inaka/sumo_db/pull/156) ([davecaos](https://github.com/davecaos))

- \[Fixes \#150\] Added commas :boom: [\#151](https://github.com/inaka/sumo_db/pull/151) ([jfacorro](https://github.com/jfacorro))

- Upgrade tirerl to 0.1.4 [\#149](https://github.com/inaka/sumo_db/pull/149) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Fix worker\_pool dep [\#148](https://github.com/inaka/sumo_db/pull/148) ([essen](https://github.com/essen))

- Error using sumo:find to retrieve an inexistent element [\#147](https://github.com/inaka/sumo_db/pull/147) ([elbrujohalcon](https://github.com/elbrujohalcon))

- \[\#ISSUE\] Updated Changelog. [\#144](https://github.com/inaka/sumo_db/pull/144) ([cabol](https://github.com/cabol))

## [0.3.6](https://github.com/inaka/sumo_db/tree/0.3.6) (2015-04-27)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.5...0.3.6)

**Closed issues:**

- Fix 'find\_by' function on Riak store to return an empty list '\[\]' when it's invoked by ID. [\#130](https://github.com/inaka/sumo_db/issues/130)

**Merged pull requests:**

- Fixed VSN. [\#143](https://github.com/inaka/sumo_db/pull/143) ([cabol](https://github.com/cabol))

- Update LICENSE [\#142](https://github.com/inaka/sumo_db/pull/142) ([andresinaka](https://github.com/andresinaka))

- Reusable private functions were exported. Fixed sumo\_store\_riak \(enhance... [\#141](https://github.com/inaka/sumo_db/pull/141) ([cabol](https://github.com/cabol))

- Euen.test elvis [\#139](https://github.com/inaka/sumo_db/pull/139) ([Euen](https://github.com/Euen))

## [0.3.5](https://github.com/inaka/sumo_db/tree/0.3.5) (2015-04-09)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.4...0.3.5)

**Merged pull requests:**

- rebar build without ssh keys fails due to outdated dep libs [\#137](https://github.com/inaka/sumo_db/pull/137) ([Vorticity-Flux](https://github.com/Vorticity-Flux))

## [0.3.4](https://github.com/inaka/sumo_db/tree/0.3.4) (2015-04-07)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.3...0.3.4)

**Merged pull requests:**

- Add uuid as a required application [\#135](https://github.com/inaka/sumo_db/pull/135) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.3.3](https://github.com/inaka/sumo_db/tree/0.3.3) (2015-04-07)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.2...0.3.3)

**Merged pull requests:**

- Version Bump [\#134](https://github.com/inaka/sumo_db/pull/134) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Calling sumo:persist with a doc with no id field fails [\#133](https://github.com/inaka/sumo_db/pull/133) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Updated changelog [\#132](https://github.com/inaka/sumo_db/pull/132) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.3.2](https://github.com/inaka/sumo_db/tree/0.3.2) (2015-04-06)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.1...0.3.2)

**Merged pull requests:**

- \#130 Fix find\_by ID function in sumo\_store\_riak to return empty list whe... [\#131](https://github.com/inaka/sumo_db/pull/131) ([cabol](https://github.com/cabol))

- Mnesia support [\#129](https://github.com/inaka/sumo_db/pull/129) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Igaray.126.sumo db sql extras reference [\#127](https://github.com/inaka/sumo_db/pull/127) ([igaray](https://github.com/igaray))

- Automate CHANGELOG.md [\#125](https://github.com/inaka/sumo_db/pull/125) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.3.1](https://github.com/inaka/sumo_db/tree/0.3.1) (2015-03-12)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.0...0.3.1)

**Closed issues:**

- Support for find\_by\_sql for SQL backends [\#121](https://github.com/inaka/sumo_db/issues/121)

**Merged pull requests:**

- Cabol.122.riak backend config enhancement [\#124](https://github.com/inaka/sumo_db/pull/124) ([cabol](https://github.com/cabol))

## [0.3.0](https://github.com/inaka/sumo_db/tree/0.3.0) (2015-03-04)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.2.4...0.3.0)

**Fixed bugs:**

- Remove delete function and callback from sumo\_store [\#110](https://github.com/inaka/sumo_db/issues/110)

**Closed issues:**

- Compile Fails on Fresh Install [\#112](https://github.com/inaka/sumo_db/issues/112)

**Merged pull requests:**

- Version Bump [\#120](https://github.com/inaka/sumo_db/pull/120) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Cabolanos.90.riak backend [\#119](https://github.com/inaka/sumo_db/pull/119) ([cabol](https://github.com/cabol))

- waffle.io Badge [\#118](https://github.com/inaka/sumo_db/pull/118) ([waffle-iron](https://github.com/waffle-iron))

- \[Fixed \#110\] Remove delete function and callback from sumo\_store. [\#116](https://github.com/inaka/sumo_db/pull/116) ([cabol](https://github.com/cabol))

- Move epgsql deps dir to match rebar location [\#114](https://github.com/inaka/sumo_db/pull/114) ([spiegela](https://github.com/spiegela))

- \[Fixed \#112\] Add deps to build with rebar. [\#113](https://github.com/inaka/sumo_db/pull/113) ([jfacorro](https://github.com/jfacorro))

## [0.2.4](https://github.com/inaka/sumo_db/tree/0.2.4) (2015-01-05)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.2.3...0.2.4)

**Closed issues:**

- add a sumo:sort\_fields type [\#109](https://github.com/inaka/sumo_db/issues/109)

**Merged pull requests:**

- Code cleanup [\#111](https://github.com/inaka/sumo_db/pull/111) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.3](https://github.com/inaka/sumo_db/tree/0.2.3) (2014-12-31)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.2.2...0.2.3)

**Merged pull requests:**

- Renaming dependency to avoid conflict with tirerl [\#108](https://github.com/inaka/sumo_db/pull/108) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.2](https://github.com/inaka/sumo_db/tree/0.2.2) (2014-12-30)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.2.1...0.2.2)

**Merged pull requests:**

- Don't require the import of emysql records on stores based on sumo\_store\_msyql [\#106](https://github.com/inaka/sumo_db/pull/106) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.2.1](https://github.com/inaka/sumo_db/tree/0.2.1) (2014-12-16)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.5...0.2.1)

**Fixed bugs:**

- Remove all mentions of repos for stores [\#104](https://github.com/inaka/sumo_db/issues/104)

**Merged pull requests:**

- \[Closes \#104\] Removed all references to repositories. [\#105](https://github.com/inaka/sumo_db/pull/105) ([jfacorro](https://github.com/jfacorro))

## [0.1.5](https://github.com/inaka/sumo_db/tree/0.1.5) (2014-12-15)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.4...0.1.5)

**Closed issues:**

- Bump to 0.1.5 [\#102](https://github.com/inaka/sumo_db/issues/102)

- Move Contributors section to the end of the file [\#97](https://github.com/inaka/sumo_db/issues/97)

- Rename repos to connection or store. [\#62](https://github.com/inaka/sumo_db/issues/62)

- Change internal doc representation to maps [\#60](https://github.com/inaka/sumo_db/issues/60)

- PostgreSQL storage backend [\#39](https://github.com/inaka/sumo_db/issues/39)

- add asc/desc option for ordering [\#21](https://github.com/inaka/sumo_db/issues/21)

- add specs [\#18](https://github.com/inaka/sumo_db/issues/18)

- write unit tests [\#15](https://github.com/inaka/sumo_db/issues/15)

**Merged pull requests:**

- \[Closes \#102\] [\#103](https://github.com/inaka/sumo_db/pull/103) ([jfacorro](https://github.com/jfacorro))

- \[\#39\] pgsql backend and store [\#101](https://github.com/inaka/sumo_db/pull/101) ([jfacorro](https://github.com/jfacorro))

- \[\#18\] Added all missing specs. [\#100](https://github.com/inaka/sumo_db/pull/100) ([jfacorro](https://github.com/jfacorro))

- \[\#60\] Change internal records and proplists to maps [\#99](https://github.com/inaka/sumo_db/pull/99) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#97\] Fixed broken link. Moved sections around. [\#98](https://github.com/inaka/sumo_db/pull/98) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#62\] Rename repos to store. [\#96](https://github.com/inaka/sumo_db/pull/96) ([jfacorro](https://github.com/jfacorro))

## [0.1.4](https://github.com/inaka/sumo_db/tree/0.1.4) (2014-11-04)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.3...0.1.4)

**Fixed bugs:**

- Lock the deps [\#72](https://github.com/inaka/sumo_db/issues/72)

- log query and millis [\#70](https://github.com/inaka/sumo_db/issues/70)

**Closed issues:**

- Decide between implementing SQLite repo or deleting it  [\#87](https://github.com/inaka/sumo_db/issues/87)

- Support for rails style callbacks? [\#86](https://github.com/inaka/sumo_db/issues/86)

- make all blog is erroring out   [\#80](https://github.com/inaka/sumo_db/issues/80)

- Modify the default value for the worker\_pool option overrun\_warning [\#77](https://github.com/inaka/sumo_db/issues/77)

- ElasticSearch storage backend [\#74](https://github.com/inaka/sumo_db/issues/74)

- make compatible with erlang.mk [\#61](https://github.com/inaka/sumo_db/issues/61)

- Use a proper overrun warning for wpool [\#51](https://github.com/inaka/sumo_db/issues/51)

- repos should go through their backend instead of just using the pool [\#38](https://github.com/inaka/sumo_db/issues/38)

- Improve semantics for redis repo [\#35](https://github.com/inaka/sumo_db/issues/35)

- Add an environment variable to turn on/off query debug messages [\#23](https://github.com/inaka/sumo_db/issues/23)

- Add support for full conditional logic in the find\_by and delete\_by functions. [\#13](https://github.com/inaka/sumo_db/issues/13)

- If id attribute is present in 3rd paramenter to sumo:new\_field/3, sumo\_db should assume not\_null as well. [\#9](https://github.com/inaka/sumo_db/issues/9)

- Use a parse transform to avoid having to declare the sumo\_docs manually. [\#2](https://github.com/inaka/sumo_db/issues/2)

**Merged pull requests:**

- \[\#21\] Support for sorting asc/desc [\#95](https://github.com/inaka/sumo_db/pull/95) ([jfacorro](https://github.com/jfacorro))

- \[\#51\] Better overrun defaults. [\#94](https://github.com/inaka/sumo_db/pull/94) ([jfacorro](https://github.com/jfacorro))

- \[\#87\] Removed sqlite repo and dep. [\#93](https://github.com/inaka/sumo_db/pull/93) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#61\] Added Erlang.mk [\#92](https://github.com/inaka/sumo_db/pull/92) ([jfacorro](https://github.com/jfacorro))

- \[\#67\] Open-source checklist [\#91](https://github.com/inaka/sumo_db/pull/91) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#13\] Full conditional logic [\#88](https://github.com/inaka/sumo_db/pull/88) ([jfacorro](https://github.com/jfacorro))

- README + CONTRIBUTORS [\#85](https://github.com/inaka/sumo_db/pull/85) ([elbrujohalcon](https://github.com/elbrujohalcon))

- \[\#80\] Fixed the run escript to not throw errors for make blog [\#82](https://github.com/inaka/sumo_db/pull/82) ([AxisOfEval](https://github.com/AxisOfEval))

- Misc changes to help build cleanly. [\#81](https://github.com/inaka/sumo_db/pull/81) ([AxisOfEval](https://github.com/AxisOfEval))

- Fixed rebar.config to not use the git account for github. [\#79](https://github.com/inaka/sumo_db/pull/79) ([AxisOfEval](https://github.com/AxisOfEval))

- \[Closes \#77\] Modified default value to 30 seconds. [\#78](https://github.com/inaka/sumo_db/pull/78) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#74\] ElasticSearch backend and repo [\#75](https://github.com/inaka/sumo_db/pull/75) ([jfacorro](https://github.com/jfacorro))

- \[fix \#72\] Change specifications on deps. [\#73](https://github.com/inaka/sumo_db/pull/73) ([Euen](https://github.com/Euen))

- \[fix \#70\] Query log in milliseconds. [\#71](https://github.com/inaka/sumo_db/pull/71) ([Euen](https://github.com/Euen))

## [0.1.3](https://github.com/inaka/sumo_db/tree/0.1.3) (2014-09-23)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.2...0.1.3)

**Closed issues:**

- Allow to specify worker\_pool options in configuration [\#68](https://github.com/inaka/sumo_db/issues/68)

- Add an update function to the mysql repo.  [\#49](https://github.com/inaka/sumo_db/issues/49)

**Merged pull requests:**

- \[Closes \#68\] Accept `wpool\_opts` entry in configuration file. [\#69](https://github.com/inaka/sumo_db/pull/69) ([jfacorro](https://github.com/jfacorro))

## [0.1.2](https://github.com/inaka/sumo_db/tree/0.1.2) (2014-07-11)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.1...0.1.2)

**Closed issues:**

- Update rebar version so that it works with erlang.mk [\#58](https://github.com/inaka/sumo_db/issues/58)

- Upgrade lager version to 2.0.3 [\#56](https://github.com/inaka/sumo_db/issues/56)

**Merged pull requests:**

- \[\#58\] Upgraded rebar to 2.5.0 [\#59](https://github.com/inaka/sumo_db/pull/59) ([jfacorro](https://github.com/jfacorro))

## [0.1.1](https://github.com/inaka/sumo_db/tree/0.1.1) (2014-07-02)

[Full Changelog](https://github.com/inaka/sumo_db/compare/0.1.0...0.1.1)

**Merged pull requests:**

- \[\#56\] Ready for R17 [\#57](https://github.com/inaka/sumo_db/pull/57) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.1.0](https://github.com/inaka/sumo_db/tree/0.1.0) (2014-06-04)

[Full Changelog](https://github.com/inaka/sumo_db/compare/choosy.production...0.1.0)

**Closed issues:**

- change behaviour\_info to -callback [\#37](https://github.com/inaka/sumo_db/issues/37)

**Merged pull requests:**

- Naming the types, instead of the generic term\(\) or any\(\) [\#55](https://github.com/inaka/sumo_db/pull/55) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Now even the blog is dialyzed [\#54](https://github.com/inaka/sumo_db/pull/54) ([elbrujohalcon](https://github.com/elbrujohalcon))

- More dialyzing! [\#53](https://github.com/inaka/sumo_db/pull/53) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Dialyzed [\#52](https://github.com/inaka/sumo_db/pull/52) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [choosy.production](https://github.com/inaka/sumo_db/tree/choosy.production) (2014-06-02)

**Closed issues:**

- create sumo\_backend behavior [\#43](https://github.com/inaka/sumo_db/issues/43)

- sumo\_backend\_sup should call Mod:start\_link instead of gen\_server:start\_link  [\#42](https://github.com/inaka/sumo_db/issues/42)

- Improve error detection and reporting [\#14](https://github.com/inaka/sumo_db/issues/14)

- Add delete\_by [\#8](https://github.com/inaka/sumo_db/issues/8)

- Optional application dependencies. [\#6](https://github.com/inaka/sumo_db/issues/6)

- Improve execute [\#3](https://github.com/inaka/sumo_db/issues/3)

**Merged pull requests:**

- MySQL persist no longer updates id field in on duplicate clause. [\#50](https://github.com/inaka/sumo_db/pull/50) ([igaray](https://github.com/igaray))

- Fixed a prepared statement name clash. [\#48](https://github.com/inaka/sumo_db/pull/48) ([igaray](https://github.com/igaray))

- changing event name so they wont collide [\#47](https://github.com/inaka/sumo_db/pull/47) ([marcelog](https://github.com/marcelog))

- adding new conditions! [\#46](https://github.com/inaka/sumo_db/pull/46) ([marcelog](https://github.com/marcelog))

- Marcelog sql builder [\#45](https://github.com/inaka/sumo_db/pull/45) ([marcelog](https://github.com/marcelog))

- closes \#42, closes \#43 [\#44](https://github.com/inaka/sumo_db/pull/44) ([marcelog](https://github.com/marcelog))

- mongodb is back [\#36](https://github.com/inaka/sumo_db/pull/36) ([marcelog](https://github.com/marcelog))

- Corrected CLD macro to allow several storage backends of same type. [\#34](https://github.com/inaka/sumo_db/pull/34) ([igaray](https://github.com/igaray))

- changing copyright [\#33](https://github.com/inaka/sumo_db/pull/33) ([marcelog](https://github.com/marcelog))

- Added a configurable timeout to the worker pool call. [\#32](https://github.com/inaka/sumo_db/pull/32) ([igaray](https://github.com/igaray))

- Inaki.worker pool [\#31](https://github.com/inaka/sumo_db/pull/31) ([igaray](https://github.com/igaray))

- Inaki.conditional log [\#30](https://github.com/inaka/sumo_db/pull/30) ([igaray](https://github.com/igaray))

- Those were microseconds [\#29](https://github.com/inaka/sumo_db/pull/29) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Added env var to specify emysql pool size. [\#28](https://github.com/inaka/sumo_db/pull/28) ([igaray](https://github.com/igaray))

- Timed execute in mysql\_repo [\#27](https://github.com/inaka/sumo_db/pull/27) ([igaray](https://github.com/igaray))

- Fixed argument list in find\_all. [\#26](https://github.com/inaka/sumo_db/pull/26) ([igaray](https://github.com/igaray))

- delete\_by was not joining conditions with interspaced AND. [\#25](https://github.com/inaka/sumo_db/pull/25) ([igaray](https://github.com/igaray))

- closes \#8 [\#24](https://github.com/inaka/sumo_db/pull/24) ([igaray](https://github.com/igaray))

- Marcelog delete by 8 [\#22](https://github.com/inaka/sumo_db/pull/22) ([marcelog](https://github.com/marcelog))

- not imposing dependencies anymore, closes \#6 [\#20](https://github.com/inaka/sumo_db/pull/20) ([marcelog](https://github.com/marcelog))

- Marcelog better execute 3 [\#19](https://github.com/inaka/sumo_db/pull/19) ([marcelog](https://github.com/marcelog))

- Fixed pattern matching in sumo:call [\#17](https://github.com/inaka/sumo_db/pull/17) ([igaray](https://github.com/igaray))

- Marcelog error reporting 14 [\#16](https://github.com/inaka/sumo_db/pull/16) ([marcelog](https://github.com/marcelog))

- Coverted find\_all results with sumo\_wakeup. [\#12](https://github.com/inaka/sumo_db/pull/12) ([igaray](https://github.com/igaray))

- Inaki.find all [\#11](https://github.com/inaka/sumo_db/pull/11) ([igaray](https://github.com/igaray))

- WHERE clauses must be joined with AND instead of commas [\#10](https://github.com/inaka/sumo_db/pull/10) ([elbrujohalcon](https://github.com/elbrujohalcon))

- Small fix to make non integer id fields work. [\#7](https://github.com/inaka/sumo_db/pull/7) ([igaray](https://github.com/igaray))

- Fixed a bug affecting persistence of non-proplist sumo\_docs. [\#5](https://github.com/inaka/sumo_db/pull/5) ([igaray](https://github.com/igaray))

- Added date types to the mysql repo. [\#4](https://github.com/inaka/sumo_db/pull/4) ([igaray](https://github.com/igaray))

- Changed sumo\_repo\_sup to ask for the sumo\_db applicaiton environment. [\#1](https://github.com/inaka/sumo_db/pull/1) ([igaray](https://github.com/igaray))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*