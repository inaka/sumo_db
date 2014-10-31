PROJECT = sumo_db

DEPS = lager emysql emongo tirerl worker_pool

dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_emysql = git https://github.com/Eonblast/Emysql.git v0.4.1
dep_emongo = git https://github.com/inaka/emongo.git v0.2.1
dep_tirerl = git https://github.com/inaka/tirerl 0.1.0
dep_worker_pool = git https://github.com/inaka/worker_pool.git 1.0

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = conditional_logic
CT_OPTS = -s emysql -s sumo_db -erl_args -config test/test.config

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s sync -s emysql -s sumo_db -config test/test.config

erldocs:
	erldocs . -o docs
