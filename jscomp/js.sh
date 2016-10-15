#!/bin/sh
set -e 
## building js 
# ocamlbuild -use-ocamlfind -no-hygiene  -syntax camlp4o -pkgs js_of_ocaml,js_of_ocaml.syntax,js_of_ocaml.toplevel exports.byte
FILE=jsoo_main.byte


# ocamlbuild -lflags -no-check-prims -use-ocamlfind -I bin -no-hygiene   -pkgs compiler-libs.common -no-links $FILE

# jsoo_mkcmis stdlib
# js_of_ocaml -I +compiler-libs   --toplevel +weak.js +toplevel.js  ./polyfill.js _build/$FILE -I ./others/ -I ./runtime/ --file js.cmi:/cmis/js.cmi --file js_unsafe.cmi:/cmis/js_unsafe.cmi --file js_re.cmi:/cmis/js_re.cmi --file js_array.cmi:/cmis/js_array.cmi --file js_null.cmi:/cmis/js_null.cmi --file js_undefined.cmi:/cmis/js_undefined.cmi --file js_types.cmi:/cmis/js_types.cmi --file js_null_undefined.cmi:/cmis/js_null_undefined.cmi --file js_dict.cmi:/cmis/js_dict.cmi --file js_string.cmi:/cmis/js_string.cmi   -o _build/exports.js 

BS_COMPILER_IN_BROWSER=true bspack.exe -bs-log-mllib bin/js_compiler.mllib -prelude-str 'module Config = Config_whole_compiler' -bs-exclude-I config -o bin/js_compiler.ml -bs-main jsoo_main.ml -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ../ocaml/driver/ -I ext -I syntax -I depends -I common


ocamlc.opt -w -a -I bin bin/config_whole_compiler.mli bin/config_whole_compiler.ml bin/js_compiler.mli bin/js_compiler.ml -no-check-prims -o bin/js_compiler.byte

DOCS_DIR=../docs/js-demo

js_of_ocaml  --toplevel +weak.js +toplevel.js  ./polyfill.js bin/js_compiler.byte -I ./others/ -I ./runtime/ --file js.cmi:/cmis/js.cmi --file js_unsafe.cmi:/cmis/js_unsafe.cmi --file js_re.cmi:/cmis/js_re.cmi --file js_array.cmi:/cmis/js_array.cmi --file js_null.cmi:/cmis/js_null.cmi --file js_undefined.cmi:/cmis/js_undefined.cmi --file js_types.cmi:/cmis/js_types.cmi --file js_null_undefined.cmi:/cmis/js_null_undefined.cmi --file js_dict.cmi:/cmis/js_dict.cmi --file js_string.cmi:/cmis/js_string.cmi   -o $DOCS_DIR/exports.js


ocamlbuild -use-ocamlfind -no-hygiene -no-links js_generate_require.byte --
rm -rf  $DOCS_DIR/pre_load.js && cp ./pre_load.js $DOCS_DIR
# TODO: build with amd first 
cp ../lib/amdjs/*.js $DOCS_DIR/stdlib

# note it is preferred to run ./release.sh && ./js.sh otherwise  amdjs is not really latest









