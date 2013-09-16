CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
REBAR?=./rebar

all: getdeps compile edoc
	${REBAR} compile

blog:
	(cd examples/blog && ./run)

edoc:
	${REBAR} doc

getdeps:
	${REBAR} get-deps

compile:
	${REBAR} compile

test: compile
	${REBAR} eunit skip_deps=true

clean:
	${REBAR} clean

