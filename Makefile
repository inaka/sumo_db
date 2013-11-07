CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
REBAR?=./rebar

all: getdeps compile edoc
	${REBAR} compile

blog:
	${REBAR} -C examples/blog/rebar.config get
	${REBAR} -C examples/blog/rebar.config compile
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

