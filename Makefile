CWD=$(shell pwd)
NAME?=$(shell basename ${CWD})
ROOT?=${CWD}
REBAR?=./rebar
ERL?=/usr/bin/env erl
HOST?=$(shell hostname)
NODE?=${NAME}@${HOST}

ERL_ARGS?=-pa ebin -pa deps/*/ebin -name ${NODE}

all: getdeps compile edoc
	${REBAR} compile

blog:
	${REBAR} -C examples/blog/rebar.config get
	${REBAR} -C examples/blog/rebar.config compile
	(cd examples/blog && ./run)

shell: compile_no_deps
	${ERL} ${ERL_ARGS}

edoc:
	${REBAR} doc

getdeps:
	${REBAR} get-deps

compile_no_deps:
	${REBAR} compile skip_deps=true

compile:
	${REBAR} compile

test: compile
	${REBAR} eunit skip_deps=true

clean:
	${REBAR} clean

