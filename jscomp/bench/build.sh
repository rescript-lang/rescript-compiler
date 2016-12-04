#!/bin/sh
set -e
ocamlc.opt -g bal_set_mini.ml -o bal_set_mini.byte && js_of_ocaml bal_set_mini.byte -o bal_set_mini.jsoo.js
ocamlopt.opt -g bal_set_mini.ml -o bal_set_mini.native
bscc -c bal_set_mini.ml
java -jar /usr/local/lib/node_modules/google-closure-compiler/compiler.jar  --compilation_level ADVANCED --js bal_set_mini.js --js_output_file bal_set_mini.goog.js
time node immutable_set_min.js
time node bal_set_mini.goog.js
time node bal_set_mini.jsoo.js
time bal_set_mini.byte
time bal_set_mini.native
