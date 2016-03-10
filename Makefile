PROJECT = sumo_db

CONFIG ?= test/test.config

DEPS = lager uuid worker_pool
SHELL_DEPS = sync

dep_sync        = git https://github.com/rustyio/sync.git      9c78e7b
dep_lager       = git https://github.com/basho/lager.git       3.0.1
dep_worker_pool = git https://github.com/inaka/worker_pool.git 1.0.4
dep_uuid        = git https://github.com/okeuday/uuid.git      31f408f4ef

TEST_DEPS = katana_test mixer
dep_katana_test = git https://github.com/inaka/katana-test.git 5491fba
dep_mixer = git git://github.com/inaka/mixer.git 0.1.5

CT_SUITES ?= conditional_logic sumo_basic sumo_events sumo_meta

include erlang.mk

LOCAL_DEPS := tools common_test crypto test_server mnesia
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

COMPILE_FIRST += sumo_backend sumo_doc sumo_store

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info
CT_OPTS = -cover test/sumo.coverspec -vvv -erl_args -boot start_sasl -config ${CONFIG}

SHELL_OPTS = -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s sync

test-shell: test-build app
	erl  -name ${PROJECT}@`hostname` -pa ebin -pa deps/*/ebin -pa test -s lager -s sync -config ${CONFIG}

erldocs:
	erldocs . -o docs

changelog:
	github_changelog_generator --token ${TOKEN}

EDOC_OPTS += todo, report_missing_types

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze
