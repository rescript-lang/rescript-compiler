SHELL = /bin/bash

build:
	dune build
	./scripts/copyExes.js

watch:
	dune build -w

bench:
	dune exec -- syntax_benchmarks

dce:
	opam exec reanalyze.exe -- -dce-cmt _build

ninja/ninja:
	./scripts/buildNinjaBinary.js

ninja: ninja/ninja

test: lib
	dune exec -- node scripts/ciTest.js -all

test-syntax: build
	dune exec -- syntax_tests
	dune exec -- bash ./scripts/test_syntax.sh
	make reanalyze
	bash ./scripts/testok.sh

test-syntax-roundtrip: build
	dune exec -- syntax_tests
	ROUNDTRIP_TEST=1 dune exec -- bash ./scripts/test.sh
	make reanalyze
	bash ./scripts/testok.sh

test-gentype: build
	make -C jscomp/gentype_tests/typescript-react-example test

test-all: test test-gentype

reanalyze:
	reanalyze.exe -set-exit-code -all-cmt _build/default/res_syntax -suppress res_syntax/testrunner,res_syntax/compiler-libs-406 -exclude-paths res_syntax/compiler-libs-406

lib: build ninja/ninja
	dune exec -- node scripts/ninja.js config
	dune exec -- node scripts/ninja.js build

artifacts: lib
	./scripts/prebuilt.js
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
