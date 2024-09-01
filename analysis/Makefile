SHELL = /bin/bash

build-tests:
	make -C tests build

build-tests-generic-jsx-transform:
	make -C tests-generic-jsx-transform build

build-tests-incremental-typechecking:
	make -C tests-incremental-typechecking build

build-reanalyze:
	make -C reanalyze build

build: build-reanalyze build-tests build-tests-generic-jsx-transform build-tests-incremental-typechecking

dce: build-analysis-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

test-analysis-binary:
	make -C tests test
	make -C tests-generic-jsx-transform test
	make -C tests-incremental-typechecking test

test-reanalyze:
	make -C reanalyze test

test: test-analysis-binary test-reanalyze

clean:
	make -C tests clean
	make -C tests-generic-jsx-transform clean
	make -C tests-incremental-typechecking clean
	make -C reanalyze clean

.PHONY: build-reanalyze build-tests dce clean test
