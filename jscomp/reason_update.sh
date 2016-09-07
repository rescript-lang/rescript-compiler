#!/bin/sh
set -e
cd ../reason/src/ && ocamllex reason_lexer.mll && menhir reason_parser.mly && cd ../../jscomp

# better to make it explicit
bspack -bs-mllib bin/reason.mllib -o bin/reason.ml

rm ../reason/src/reason_lexer.ml ../reason/src/reason_parser.ml ../reason/src/reason_parser.mli

