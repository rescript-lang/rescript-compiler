#!/bin/zsh

# pack and parse the whole codebase using the compiler itself. Kind of a test
rm -rf ./bootstrap
mkdir ./bootstrap

ocaml unix.cma ./scripts/bspack.ml -bs-main Res_cli -I cli -I src -o ./bootstrap/rescript.ml
rescript ./bootstrap/rescript.ml > ./bootstrap/rescript.res
ocamlopt.opt -w a -pp "rescript -print binary" -O2 -o rescript -I +compiler-libs ocamlcommon.cmxa -I lib -impl ./bootstrap/rescript.res
