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

lib: build
	node scripts/install -force-lib-rebuild

artifacts: lib
	./scripts/makeArtifactList.js

clean-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean

clean:
	dune clean
	./scripts/ninja.js clean

clean-all: clean clean-gentype

.DEFAULT_GOAL := build

.PHONY: build clean clean-gentype clean-all config lib test test-all test-gentype
