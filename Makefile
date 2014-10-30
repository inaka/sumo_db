CWD=$(shell pwd)
NAME?=$(shell basename ${CWD})
ROOT?=${CWD}
REBAR?=./rebar
ERL?=/usr/bin/env erl
HOST?=$(shell hostname)
NODE?=${NAME}@${HOST}
DIALYZER_OUT?=${NAME}.plt

ERL_ARGS?=-pa ebin -pa deps/*/ebin -name ${NODE}

CT_SUITES?=conditional_logic

all: getdeps compile
	${REBAR} compile

blog:
	${REBAR} -C examples/blog/rebar.config get
	${REBAR} -C examples/blog/rebar.config compile
	(cd examples/blog && ./run)

shell: compile_no_deps
	${ERL} ${ERL_ARGS}

edoc:
	${REBAR} skip_deps=true doc

getdeps:
	${REBAR} get-deps

compile_no_deps:
	${REBAR} compile skip_deps=true

compile:
	${REBAR} compile

tests: compile
	${REBAR} skip_deps=true ct
	open log/ct/index.html

clean:
	${REBAR} clean

${DIALYZER_OUT}:
	dialyzer --verbose --build_plt -pa deps/*/ebin --output_plt ${DIALYZER_OUT} \
	 --apps stdlib erts compiler crypto edoc gs syntax_tools tools runtime_tools \
	 inets xmerl ssl mnesia webtool kernel

analyze: compile ${DIALYZER_OUT} xref
	dialyzer --verbose --plt ${DIALYZER_OUT} -Werror_handling `find ebin -name "*.beam" | grep -v SUITE`

xref:
	${REBAR} skip_deps=true --verbose compile xref
