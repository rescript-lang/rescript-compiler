#!/bin/zsh

# pack and parse the whole codebase using the compiler itself. Kind of a test
rm -rf ./bootstrap
mkdir ./bootstrap

ocaml unix.cma ./scripts/bspack.ml -bs-main Res_cli -I cli -I src -o ./bootstrap/res_parser.ml
res_parser ./bootstrap/res_parser.ml > ./bootstrap/res_parser.res
ocamlopt.opt -w a -pp "res_parser -print binary" -O2 -o res_parser -I +compiler-libs ocamlcommon.cmxa -I lib -impl ./bootstrap/res_parser.res
