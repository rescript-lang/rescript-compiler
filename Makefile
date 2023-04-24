SHELL = /bin/bash

DUNE_BIN_DIR = ./_build/install/default/bin

build: ninja
	dune build
	./scripts/copyExes.js

watch:
	dune build -w

bench:
	$(DUNE_BIN_DIR)/syntax_benchmarks

dce:
	reanalyze.exe -dce-cmt _build/default/jscomp

ninja/ninja:
	./scripts/buildNinjaBinary.js

ninja: ninja/ninja

node_modules/.bin/semver:
	npm install

test: lib
	node scripts/ciTest.js -all

test-syntax:
	bash ./scripts/test_syntax.sh
	make reanalyze
	bash ./scripts/testok.sh

test-syntax-roundtrip:
	ROUNDTRIP_TEST=1 bash ./scripts/test_syntax.sh
	make reanalyze
	bash ./scripts/testok.sh

test-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean test

test-all: test test-gentype

reanalyze:
	reanalyze.exe -set-exit-code -all-cmt _build/default/res_syntax -suppress res_syntax/testrunner
	reanalyze.exe -set-exit-code -all-cmt _build/default/jscomp -suppress res_syntax/testrunner -exclude-paths jscomp/super_errors,jscomp/outcome_printer,jscomp/ounit_tests,jscomp/ml,jscomp/js_parser,jscomp/frontend,jscomp/ext,jscomp/depends,jscomp/core,jscomp/common,jscomp/cmij,jscomp/bsb_helper,jscomp/bsb

lib: build node_modules/.bin/semver
	node scripts/ninja.js config
	node scripts/ninja.js build
	./scripts/prebuilt.js

artifacts: lib
	./scripts/makeArtifactList.js

format:
	dune build @fmt --auto-promote

checkformat:
	dune build @fmt

clean-gentype:
	make -C jscomp/gentype_tests/typescript-react-example clean

clean:
	dune clean
	./scripts/ninja.js clean

clean-all: clean clean-gentype

.DEFAULT_GOAL := build

.PHONY: build watch ninja bench dce test test-syntax test-syntax-roundtrip test-gentype test-all lib artifacts format checkformat clean-gentype clean clean-all
