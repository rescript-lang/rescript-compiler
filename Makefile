SHELL = /bin/bash

config:
	./scripts/ninja.js config

build: config
	./scripts/ninja.js build
	dune build

watch: config
	dune build -w

dce: build
	opam exec reanalyze.exe -- -dce-cmt _build

test: build
	npm test

test-gentype: build
	make -C jscomp/gentype_tests/typescript-react-example test

test-all: test test-gentype

clean-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean

clean:
	dune clean
	./scripts/ninja.js clean

clean-all: clean clean-gentype

.DEFAULT_GOAL := build

.PHONY: clean clean-gentype clean-all config build test test-gentype test-all
