# Change Log

## [0.7.2](https://github.com/inaka/sumo_db/tree/0.7.2) (2017-06-06)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.7.1...0.7.2)

**Fixed bugs:**

- Function delete/3 not implemented in postgres store [\#163](https://github.com/inaka/sumo_db/issues/163)
- example/blog/run :  {error,{"no such file or directory","worker\_pool.app"}} [\#146](https://github.com/inaka/sumo_db/issues/146)
- PostgreSql: position of not\_null attribute in field schema definition  [\#138](https://github.com/inaka/sumo_db/issues/138)
- Ensure all backends respect the log\_queries option [\#136](https://github.com/inaka/sumo_db/issues/136)

**Closed issues:**

- Move src/adapter\_test\_helpers folder out from test folder, let sumo src agnostic to test helpers [\#299](https://github.com/inaka/sumo_db/issues/299)
- Add links to adapters in documentation [\#288](https://github.com/inaka/sumo_db/issues/288)
- Remove the need for mnesia's start\_phase magic [\#275](https://github.com/inaka/sumo_db/issues/275)
- Modify events config to follow the same form as docs [\#268](https://github.com/inaka/sumo_db/issues/268)
- Remove unnecessary macros [\#236](https://github.com/inaka/sumo_db/issues/236)
- Document mnesia backend requierments [\#214](https://github.com/inaka/sumo_db/issues/214)
- Riak Store: implement sort for sumo:find\_by/5 [\#200](https://github.com/inaka/sumo_db/issues/200)
- Update epgsql to 3.1.0 [\#172](https://github.com/inaka/sumo_db/issues/172)
- Use named statements to improve performance in PostgreSQL store [\#171](https://github.com/inaka/sumo_db/issues/171)
- Have the riak store create bucket types in create\_schema [\#145](https://github.com/inaka/sumo_db/issues/145)
- Make the build be able to fetch dependencies dynamically \(on-demand\), for DB providers \(MySQL, PostgreSQL, MongoDB, etc.\). [\#117](https://github.com/inaka/sumo_db/issues/117)
- Implement sumo\_store\_elasticsearch:find\_by/6 [\#107](https://github.com/inaka/sumo_db/issues/107)
- Full conditional logic support for ElasticSearch  [\#89](https://github.com/inaka/sumo_db/issues/89)
- ElasticSearch: take into account other mapping options when creating a schema  [\#76](https://github.com/inaka/sumo_db/issues/76)
- Fulfill the open-source checklist [\#67](https://github.com/inaka/sumo_db/issues/67)

**Merged pull requests:**

- \[\#295\] Implement sumo\_changeset – analogous to Elixir Ecto.Changeset [\#306](https://github.com/inaka/sumo_db/pull/306) ([cabol](https://github.com/cabol))
- \[WIP\] Remove dependency on sasl [\#305](https://github.com/inaka/sumo_db/pull/305) ([lucafavatella](https://github.com/lucafavatella))
- Do not depend on lager [\#304](https://github.com/inaka/sumo_db/pull/304) ([lucafavatella](https://github.com/lucafavatella))
- sumo:fetch/2 replaced sumo:find/2 in /examples/blog/src/blog.erl [\#302](https://github.com/inaka/sumo_db/pull/302) ([nayibor](https://github.com/nayibor))
- \[\#136\] – Ensure all backends respect the log\_queries option [\#300](https://github.com/inaka/sumo_db/pull/300) ([cabol](https://github.com/cabol))
- \[\#275\] – Remove the need for mnesia's start\_phase magic [\#298](https://github.com/inaka/sumo_db/pull/298) ([cabol](https://github.com/cabol))
- Enforce that schema has at least a field with at least `id` attribute [\#294](https://github.com/inaka/sumo_db/pull/294) ([lucafavatella](https://github.com/lucafavatella))
- Refactor sumo\_internal:get\_id\_field [\#293](https://github.com/inaka/sumo_db/pull/293) ([lucafavatella](https://github.com/lucafavatella))
- Mention guidelines [\#292](https://github.com/inaka/sumo_db/pull/292) ([lucafavatella](https://github.com/lucafavatella))
- Plant CI [\#291](https://github.com/inaka/sumo_db/pull/291) ([lucafavatella](https://github.com/lucafavatella))
- Avoid silently throwing error [\#290](https://github.com/inaka/sumo_db/pull/290) ([lucafavatella](https://github.com/lucafavatella))
- Delete wrong comment in mnesia adapter [\#289](https://github.com/inaka/sumo_db/pull/289) ([lucafavatella](https://github.com/lucafavatella))

## [0.7.1](https://github.com/inaka/sumo_db/tree/0.7.1) (2016-10-14)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.7.0...0.7.1)

**Closed issues:**

- Version Bump to 0.7.1 [\#286](https://github.com/inaka/sumo_db/issues/286)
- Link events and pre\_events [\#284](https://github.com/inaka/sumo_db/issues/284)
- Version Bump to 0.7.0 [\#282](https://github.com/inaka/sumo_db/issues/282)

**Merged pull requests:**

- \[\#286\] Version Bump to 0.7.1 [\#287](https://github.com/inaka/sumo_db/pull/287) ([ferigis](https://github.com/ferigis))
- \[\#284\] link events and pre events, improve README [\#285](https://github.com/inaka/sumo_db/pull/285) ([ferigis](https://github.com/ferigis))

## [0.7.0](https://github.com/inaka/sumo_db/tree/0.7.0) (2016-10-11)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.6.4...0.7.0)

**Closed issues:**

- Fix sumo to support that one event manager to handle multiple docs and vice versa [\#280](https://github.com/inaka/sumo_db/issues/280)

**Merged pull requests:**

- \[\#282\] Version Bump to 0.7.0 [\#283](https://github.com/inaka/sumo_db/pull/283) ([ferigis](https://github.com/ferigis))
- Cabol.280.fixes and improvements on sumo events [\#281](https://github.com/inaka/sumo_db/pull/281) ([cabol](https://github.com/cabol))
- Add pre\_events and their dispatching [\#262](https://github.com/inaka/sumo_db/pull/262) ([llamallamaduck](https://github.com/llamallamaduck))

## [0.6.4](https://github.com/inaka/sumo_db/tree/0.6.4) (2016-09-29)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.6.3...0.6.4)

**Closed issues:**

- Version Bump to 0.6.4 [\#278](https://github.com/inaka/sumo_db/issues/278)

**Merged pull requests:**

- \[Close \#278\] version bump to 0.6.4  [\#279](https://github.com/inaka/sumo_db/pull/279) ([Euen](https://github.com/Euen))

## [0.6.3](https://github.com/inaka/sumo_db/tree/0.6.3) (2016-09-29)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.6.2...0.6.3)

**Closed issues:**

- version bump 0.6.3 [\#276](https://github.com/inaka/sumo_db/issues/276)
- Rebar3 doesn't understand our behaviors [\#271](https://github.com/inaka/sumo_db/issues/271)

**Merged pull requests:**

- \[Close \#276\] version bump to 0.6.3 [\#277](https://github.com/inaka/sumo_db/pull/277) ([Euen](https://github.com/Euen))

## [0.6.2](https://github.com/inaka/sumo_db/tree/0.6.2) (2016-09-28)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.6.1...0.6.2)

**Fixed bugs:**

- Riak store change type in maps [\#199](https://github.com/inaka/sumo_db/issues/199)

**Closed issues:**

- Version Bump to 0.6.2 [\#273](https://github.com/inaka/sumo_db/issues/273)
- Compile fails with "behaviour sumo\_store undefined" [\#270](https://github.com/inaka/sumo_db/issues/270)
- Split `find\_by` function in order to have two explicit functions: `fetch` and `find\_by` [\#265](https://github.com/inaka/sumo_db/issues/265)
- Wrong event when persisting [\#241](https://github.com/inaka/sumo_db/issues/241)
- Improve sumo in order to have a plugable architecture \(separate API from specific implementations\) [\#221](https://github.com/inaka/sumo_db/issues/221)
- Update riak dep [\#209](https://github.com/inaka/sumo_db/issues/209)

**Merged pull requests:**

- \[Close \#273\] version bump 0.6.2 [\#274](https://github.com/inaka/sumo_db/pull/274) ([Euen](https://github.com/Euen))
- \[\#271\] Changed behavior by behaviour [\#272](https://github.com/inaka/sumo_db/pull/272) ([demian711](https://github.com/demian711))
- Cabol.265.refactor\_sumo\_find\_by [\#266](https://github.com/inaka/sumo_db/pull/266) ([cabol](https://github.com/cabol))

## [0.6.1](https://github.com/inaka/sumo_db/tree/0.6.1) (2016-08-26)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.6.0...0.6.1)

**Fixed bugs:**

- Don't persist Strings as Binaries in Mnesia store [\#244](https://github.com/inaka/sumo_db/issues/244)

**Closed issues:**

- Version Bump to 0.6.1 [\#263](https://github.com/inaka/sumo_db/issues/263)
- fix sumo:create\_schema/0 [\#258](https://github.com/inaka/sumo_db/issues/258)
- fix sumo\_find\_test\_helper with the last sumo\_db changes [\#257](https://github.com/inaka/sumo_db/issues/257)
- Fix the blog example regarding with new architectural changes in `sumo\_db` [\#245](https://github.com/inaka/sumo_db/issues/245)
- Move from erlang.mk to rebar3 [\#242](https://github.com/inaka/sumo_db/issues/242)

**Merged pull requests:**

- \[Close \#263\] version bump to 263 [\#264](https://github.com/inaka/sumo_db/pull/264) ([Euen](https://github.com/Euen))
- \[\#257\] find\_test\_helper adapted to last changes in sumo\_db [\#261](https://github.com/inaka/sumo_db/pull/261) ([ferigis](https://github.com/ferigis))
- \[\#258\] create\_schema/0 fixed and test added [\#260](https://github.com/inaka/sumo_db/pull/260) ([ferigis](https://github.com/ferigis))
- Ferigis.245.fix examples [\#259](https://github.com/inaka/sumo_db/pull/259) ([ferigis](https://github.com/ferigis))

## [0.6.0](https://github.com/inaka/sumo_db/tree/0.6.0) (2016-08-17)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.5.0-OTP19...0.6.0)

**Closed issues:**

- Version Bump to 0.6.0 [\#255](https://github.com/inaka/sumo_db/issues/255)
- Simplify strings handling in `sumo` – keep `string` data type only. [\#251](https://github.com/inaka/sumo_db/issues/251)
- Support `boolean` data type. [\#250](https://github.com/inaka/sumo_db/issues/250)
- Support for new `custom` data types to `mnesia` adapter [\#248](https://github.com/inaka/sumo_db/issues/248)
- Add extra options to doc configuration [\#239](https://github.com/inaka/sumo_db/issues/239)
- Unify sumo\_db-riak-extras with sumo\_db [\#212](https://github.com/inaka/sumo_db/issues/212)
- Add generic type custom\(\) [\#186](https://github.com/inaka/sumo_db/issues/186)
- Fix `find\_by` function in `sumo\_store\_riak` to return all results when no pagination parameters are specified [\#176](https://github.com/inaka/sumo_db/issues/176)
- Modify test suites to avoid having to install all supported DBs in the dev env [\#115](https://github.com/inaka/sumo_db/issues/115)

**Merged pull requests:**

- \[Close \#255\] version bump to 0.6.0 [\#256](https://github.com/inaka/sumo_db/pull/256) ([Euen](https://github.com/Euen))
- \[Fix \#250\] Support `boolean` data type. [\#254](https://github.com/inaka/sumo_db/pull/254) ([cabol](https://github.com/cabol))
- \[Fix \#248\] Support for new `custom` data type to `mnesia`. [\#253](https://github.com/inaka/sumo_db/pull/253) ([cabol](https://github.com/cabol))
- \[Fix \#251\] Simplify strings handling. [\#252](https://github.com/inaka/sumo_db/pull/252) ([cabol](https://github.com/cabol))
- \[Fix \#239\] Add extra options to doc configuration. [\#249](https://github.com/inaka/sumo_db/pull/249) ([cabol](https://github.com/cabol))
- Cabol.242.rebar3 [\#246](https://github.com/inaka/sumo_db/pull/246) ([cabol](https://github.com/cabol))

## [0.5.0-OTP19](https://github.com/inaka/sumo_db/tree/0.5.0-OTP19) (2016-06-28)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.5.0...0.5.0-OTP19)

**Closed issues:**

- Hex Package [\#208](https://github.com/inaka/sumo_db/issues/208)

**Merged pull requests:**

- Make it compatible with OTP19 [\#243](https://github.com/inaka/sumo_db/pull/243) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Update LICENSE [\#240](https://github.com/inaka/sumo_db/pull/240) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.5.0](https://github.com/inaka/sumo_db/tree/0.5.0) (2016-03-21)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.4.0...0.5.0)

**Closed issues:**

- Update repo and make it ready for hex.pm [\#237](https://github.com/inaka/sumo_db/issues/237)
- sumo\_basic\_SUITE:check\_proper\_dates/1 not working with MongoDB backend [\#226](https://github.com/inaka/sumo_db/issues/226)
- `sumo:delete\_all/2` does not return the delete count when using MongoDB [\#225](https://github.com/inaka/sumo_db/issues/225)
- Add fix to README.md for MongoDB-CR authentication failure [\#224](https://github.com/inaka/sumo_db/issues/224)
- Fix find\_by functions in Riak Store to work with different data types, not only binaries. [\#222](https://github.com/inaka/sumo_db/issues/222)

**Merged pull requests:**

- \[Fix \#237\] Update dependencies; Update erlang.mk; Set elvis to use rulesets; Remove xref.config [\#238](https://github.com/inaka/sumo_db/pull/238) ([harenson](https://github.com/harenson))
- sumo architecture enhancements [\#235](https://github.com/inaka/sumo_db/pull/235) ([cabol](https://github.com/cabol))
- \[\#211\] Fix adapters tests helpers [\#234](https://github.com/inaka/sumo_db/pull/234) ([harenson](https://github.com/harenson))

## [0.4.0](https://github.com/inaka/sumo_db/tree/0.4.0) (2016-02-26)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.13...0.4.0)

**Fixed bugs:**

- Remove extra fields from search results before wakeup [\#206](https://github.com/inaka/sumo_db/issues/206)
- Get riak store to return the number of deleted rows in sumo:delete\_by\(\) [\#204](https://github.com/inaka/sumo_db/issues/204)
- Allow Sumo Riak Store to use nullable fields [\#203](https://github.com/inaka/sumo_db/issues/203)
- Riak is not correctly matching by id [\#192](https://github.com/inaka/sumo_db/issues/192)
- Fix mongo store, broken after date/datetime general refactor [\#188](https://github.com/inaka/sumo_db/issues/188)
- Sumo is not filtering by datetime fields correctly [\#219](https://github.com/inaka/sumo_db/pull/219) ([harenson](https://github.com/harenson))

**Closed issues:**

- Bump version to 0.4.0 [\#232](https://github.com/inaka/sumo_db/issues/232)
- `sumo\_basic\_SUITE` failing because its `init\_per\_testcase` fails [\#223](https://github.com/inaka/sumo_db/issues/223)
- Move event dispatcher management inside [\#218](https://github.com/inaka/sumo_db/issues/218)
- Add Meta Testing [\#211](https://github.com/inaka/sumo_db/issues/211)
- Invalid Prepared Statements. [\#202](https://github.com/inaka/sumo_db/issues/202)
- Sumo store have to work with other data types in the same way is doing date types persistence. [\#201](https://github.com/inaka/sumo_db/issues/201)
- Quit using inaka's emongo and switch to the official one [\#184](https://github.com/inaka/sumo_db/issues/184)

**Merged pull requests:**

- \[Fix \#232\] Bump version to 0.4.0 [\#233](https://github.com/inaka/sumo_db/pull/233) ([harenson](https://github.com/harenson))
- \[\#192\] Make Riak store take care of every condition when building the query [\#231](https://github.com/inaka/sumo_db/pull/231) ([harenson](https://github.com/harenson))
- \[Fix \#211\] Add meta testing and Fix dialyzer warnings [\#230](https://github.com/inaka/sumo_db/pull/230) ([harenson](https://github.com/harenson))
- \[Fix \#203\] Fix null values for every store [\#229](https://github.com/inaka/sumo_db/pull/229) ([harenson](https://github.com/harenson))
- \[Fix \#201\] Add checks for all sumo data types in tests [\#228](https://github.com/inaka/sumo_db/pull/228) ([harenson](https://github.com/harenson))
- Cabol.222.fix riak to work with data types [\#227](https://github.com/inaka/sumo_db/pull/227) ([cabol](https://github.com/cabol))
- \[Fix \#218\] Add event dispatch management [\#220](https://github.com/inaka/sumo_db/pull/220) ([harenson](https://github.com/harenson))
- Tests for \#216 [\#217](https://github.com/inaka/sumo_db/pull/217) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fixed issue UNIQUE constraint in sumo\_store\_pgsql [\#216](https://github.com/inaka/sumo_db/pull/216) ([antik486](https://github.com/antik486))
- Upgrade erlang.mk and get the app to compile [\#215](https://github.com/inaka/sumo_db/pull/215) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Unified sumo\_db-riak-extras project with sumo\_db [\#213](https://github.com/inaka/sumo_db/pull/213) ([cabol](https://github.com/cabol))

## [0.3.13](https://github.com/inaka/sumo_db/tree/0.3.13) (2015-09-14)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.12...0.3.13)

**Fixed bugs:**

- Sumo Riak sometimes returns a binary\(\) instead of datetime\(\) tuple [\#194](https://github.com/inaka/sumo_db/issues/194)
- Typo in sumo\_store\_mongo [\#185](https://github.com/inaka/sumo_db/issues/185)
- Fix documentation generation with erldoc [\#164](https://github.com/inaka/sumo_db/issues/164)

**Closed issues:**

- Version bump to 0.3.13 [\#197](https://github.com/inaka/sumo_db/issues/197)
- Update sumo dependencies for OTP 18 compatibility  [\#195](https://github.com/inaka/sumo_db/issues/195)

**Merged pull requests:**

- Version Bump to 0.3.13 [\#198](https://github.com/inaka/sumo_db/pull/198) ([davecaos](https://github.com/davecaos))
- \[Closes \#194\] Call wakeup after persisting document in riak [\#196](https://github.com/inaka/sumo_db/pull/196) ([jfacorro](https://github.com/jfacorro))
- \[\#138\] Add node name to test-shell target [\#193](https://github.com/inaka/sumo_db/pull/193) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#164\] Fixed docstrings that were causing errors [\#191](https://github.com/inaka/sumo_db/pull/191) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#185\] Fixed typo [\#190](https://github.com/inaka/sumo_db/pull/190) ([jfacorro](https://github.com/jfacorro))

## [0.3.12](https://github.com/inaka/sumo_db/tree/0.3.12) (2015-09-03)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.11...0.3.12)

**Fixed bugs:**

- Properly escape & quote queries in the riak store [\#177](https://github.com/inaka/sumo_db/issues/177)
- Escape queries generated by the riak backend. [\#153](https://github.com/inaka/sumo_db/issues/153)
- Inconsistent datetime handling in PostgreSQL [\#152](https://github.com/inaka/sumo_db/issues/152)

**Closed issues:**

- Modify Riak backend implementation to be able to specify buckets per store, not per backend, and in that way allow different stores can re-use the same backend. [\#122](https://github.com/inaka/sumo_db/issues/122)

**Merged pull requests:**

- Generate release [\#189](https://github.com/inaka/sumo_db/pull/189) ([Euen](https://github.com/Euen))
- Make elasticsearch backend compatible with the others [\#187](https://github.com/inaka/sumo_db/pull/187) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#182\] Fixed sumo\_store\_riak to support date/datetime fields. [\#183](https://github.com/inaka/sumo_db/pull/183) ([cabol](https://github.com/cabol))
- Correct date  and datetime in the riak store  [\#182](https://github.com/inaka/sumo_db/pull/182) ([igaray](https://github.com/igaray))
- Igaray.176.fix riak store find by default rows [\#181](https://github.com/inaka/sumo_db/pull/181) ([igaray](https://github.com/igaray))

## [0.3.11](https://github.com/inaka/sumo_db/tree/0.3.11) (2015-07-29)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.10...0.3.11)

**Closed issues:**

- Version Bump to 0.3.11 [\#179](https://github.com/inaka/sumo_db/issues/179)

**Merged pull requests:**

- Version Bump 0.3.11 [\#180](https://github.com/inaka/sumo_db/pull/180) ([davecaos](https://github.com/davecaos))
- \#177 - Fixed issue in sumo\_store\_riak query builder, properly escape/… [\#178](https://github.com/inaka/sumo_db/pull/178) ([cabol](https://github.com/cabol))

## [0.3.10](https://github.com/inaka/sumo_db/tree/0.3.10) (2015-06-25)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.9...0.3.10)

**Fixed bugs:**

- Return a different/new connection from pgsql backend   [\#170](https://github.com/inaka/sumo_db/issues/170)

**Closed issues:**

- Version Bump to 0.3.10 [\#174](https://github.com/inaka/sumo_db/issues/174)
- Update tirerl [\#166](https://github.com/inaka/sumo_db/issues/166)
- Create a pool of connections in the pgsql backend [\#123](https://github.com/inaka/sumo_db/issues/123)

**Merged pull requests:**

- \[Closes \#174\] Version bump to 0.3.10 [\#175](https://github.com/inaka/sumo_db/pull/175) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#170\] Return new connection per call to sumo\_backend\_pgsql:get\_connection [\#173](https://github.com/inaka/sumo_db/pull/173) ([jfacorro](https://github.com/jfacorro))

## [0.3.9](https://github.com/inaka/sumo_db/tree/0.3.9) (2015-06-15)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.8...0.3.9)

**Closed issues:**

- Version Bump to 0.3.9 [\#168](https://github.com/inaka/sumo_db/issues/168)

**Merged pull requests:**

- \[\#168\] Version Bump to 0.3.9 [\#169](https://github.com/inaka/sumo_db/pull/169) ([davecaos](https://github.com/davecaos))
- \[\#166\] Update Tirerl dependecy to 0.1.7 [\#167](https://github.com/inaka/sumo_db/pull/167) ([davecaos](https://github.com/davecaos))

## [0.3.8](https://github.com/inaka/sumo_db/tree/0.3.8) (2015-05-29)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.7...0.3.8)

**Closed issues:**

- Add reference to sumo\_db-sql-extras [\#126](https://github.com/inaka/sumo_db/issues/126)
- Riak backend [\#90](https://github.com/inaka/sumo_db/issues/90)

**Merged pull requests:**

- Version Bump to 0.3.8 [\#162](https://github.com/inaka/sumo_db/pull/162) ([cabol](https://github.com/cabol))
- Fixed issue \#153, escape queries generated by Riak store. [\#161](https://github.com/inaka/sumo_db/pull/161) ([cabol](https://github.com/cabol))

## [0.3.7](https://github.com/inaka/sumo_db/tree/0.3.7) (2015-05-19)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.6...0.3.7)

**Fixed bugs:**

- UPDATE statement for PostgreSQL is missing commas [\#150](https://github.com/inaka/sumo_db/issues/150)
- Error using sumo:find to retrieve an inexistent element [\#147](https://github.com/inaka/sumo_db/pull/147) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Version Bump to 0.3.7 [\#159](https://github.com/inaka/sumo_db/issues/159)
- Update rebar.config file  [\#157](https://github.com/inaka/sumo_db/issues/157)
- Update Gun dependency to 0.1.9 [\#155](https://github.com/inaka/sumo_db/issues/155)
- Please update tirerl [\#154](https://github.com/inaka/sumo_db/issues/154)
- Fix 'sumo\_riak\_store' to export utility functions which are currently private [\#140](https://github.com/inaka/sumo_db/issues/140)

**Merged pull requests:**

- \[Fix \#159\] Version bump to 0.3.7 [\#160](https://github.com/inaka/sumo_db/pull/160) ([davecaos](https://github.com/davecaos))
- \[Fix \#157\] Update tirerl 0.1.6 version in makefile and rebar.config file [\#158](https://github.com/inaka/sumo_db/pull/158) ([davecaos](https://github.com/davecaos))
- \[Fix \#154\] Update Tirerl dependnecy to 0.1.5 [\#156](https://github.com/inaka/sumo_db/pull/156) ([davecaos](https://github.com/davecaos))
- \[Fixes \#150\] Added commas :boom: [\#151](https://github.com/inaka/sumo_db/pull/151) ([jfacorro](https://github.com/jfacorro))
- Upgrade tirerl to 0.1.4 [\#149](https://github.com/inaka/sumo_db/pull/149) ([elbrujohalcon](https://github.com/elbrujohalcon))
- Fix worker\_pool dep [\#148](https://github.com/inaka/sumo_db/pull/148) ([essen](https://github.com/essen))
- \[\#ISSUE\] Updated Changelog. [\#144](https://github.com/inaka/sumo_db/pull/144) ([cabol](https://github.com/cabol))

## [0.3.6](https://github.com/inaka/sumo_db/tree/0.3.6) (2015-04-27)
[Full Changelog](https://github.com/inaka/sumo_db/compare/0.3.5...0.3.6)

**Closed issues:**

- Fix 'find\_by' function on Riak store to return an empty list '\[\]' when it's invoked by ID. [\#130](https://github.com/inaka/sumo_db/issues/130)

**Merged pull requests:**

- Fixed VSN. [\#143](https://github.com/inaka/sumo_db/pull/143) ([cabol](https://github.com/cabol))
- Update LICENSE [\#142](https://github.com/inaka/sumo_db/pull/142) ([andresinaka](https://github.com/andresinaka))
- Reusable private functions were exported. Fixed sumo\_store\_riak \(enhance... [\#141](https://github.com/inaka/sumo_db/pull/141) ([cabol](https://github.com/cabol))

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

**Fixed bugs:**

- Calling sumo:persist with a doc with no id field fails [\#133](https://github.com/inaka/sumo_db/pull/133) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Merged pull requests:**

- Version Bump [\#134](https://github.com/inaka/sumo_db/pull/134) ([elbrujohalcon](https://github.com/elbrujohalcon))
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