#!/bin/sh
set -e 
## building js 
# ocamlbuild -use-ocamlfind -no-hygiene  -syntax camlp4o -pkgs js_of_ocaml,js_of_ocaml.syntax,js_of_ocaml.toplevel exports.byte

ocamlbuild -lflags -no-check-prims -use-ocamlfind -no-hygiene   -pkgs compiler-libs.toplevel -no-links jsoo_exports.byte
js_of_ocaml -I +compiler-libs --toplevel +dynlink.js +toplevel.js +weak.js  _build/jsoo_exports.byte -o _build/exports.js 


rm -rf $BUCKLESCRIPT_DOC/js-demo/exports.js && cp _build/exports.js $BUCKLESCRIPT_DOC/js-demo/exports.js

ocamlbuild -use-ocamlfind -no-hygiene -no-links js_generate_require.byte --
rm -rf  $BUCKLESCRIPT_DOC/js-demo/pre_load.js
cp ./pre_load.js $BUCKLESCRIPT_DOC/js-demo/
# TODO: build with amd first 
cp ../lib/amdjs/*.js $BUCKLESCRIPT_DOC/js-demo/stdlib










