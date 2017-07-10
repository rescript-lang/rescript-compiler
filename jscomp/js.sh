#!/bin/sh
set -e 
./release.sh
rm bin/js_compiler.ml
make -j2 bin/jscmj.exe bin/jsgen.exe bin/js_compiler.ml
./bin/jsgen.exe --
./bin/jscmj.exe

ocamlc.opt -w -30-40 -no-check-prims -I bin  bin/config_whole_compiler.mli bin/config_whole_compiler.ml bin/js_compiler.mli bin/js_compiler.ml -o jsc.byte

## needs js_cmj_datasets, preload.js and amdjs to be update
DOCS_DIR=../docs/js-demo 


js_of_ocaml  --toplevel +weak.js   ./polyfill.js jsc.byte -I ./stdlib -I ./others/ -I ./runtime/ --file lazy.cmi:/cmis/lazy.cmi --file js.cmi:/cmis/js.cmi --file js_unsafe.cmi:/cmis/js_unsafe.cmi --file js_re.cmi:/cmis/js_re.cmi --file js_array.cmi:/cmis/js_array.cmi --file js_null.cmi:/cmis/js_null.cmi --file js_undefined.cmi:/cmis/js_undefined.cmi --file js_internal.cmi:/cmis/js_internal.cmi --file js_types.cmi:/cmis/js_types.cmi --file js_null_undefined.cmi:/cmis/js_null_undefined.cmi --file js_dict.cmi:/cmis/js_dict.cmi --file js_string.cmi:/cmis/js_string.cmi   -o $DOCS_DIR/exports.js

node ../docs/js-demo/compiler_test.js

rm -rf  $DOCS_DIR/pre_load.js && cp ./pre_load.js $DOCS_DIR
# TODO: build with amd first 
cp ../lib/amdjs/*.js $DOCS_DIR/stdlib

# note it is preferred to run ./release.sh && ./js.sh otherwise  amdjs is not really latest









