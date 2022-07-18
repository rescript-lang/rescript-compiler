SHELL = /bin/bash

config:
	./scripts/ninja.js config

build: config
	dune build
	./scripts/ninja.js build

watch: config
	dune build -w

dce: build
	opam exec reanalyze.exe -- -dce-cmt _build

test: build
	npm test

test-gentype: build
	make -C jscomp/gentype_tests/typescript-react-example test

clean:
	dune clean
	./scripts/ninja.js clean
	make -C jscomp/gentype_tests/typescript-react-example clean


.DEFAULT_GOAL := build

.PHONY: config build test test-gentype
