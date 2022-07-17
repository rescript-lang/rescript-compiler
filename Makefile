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

clean:
	dune clean
	./scripts/ninja.js clean


.DEFAULT_GOAL := build

.PHONY: config build test
