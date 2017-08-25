#!/bin/sh
set -e

# Check for required packages
hash js_of_ocaml 2>/dev/null || { echo >&2 "js_of_ocaml not found on path. Please install version 2.8.4 (although not with the buckelscript switch) and put it on your path."; exit 1; }
hash ocp-ocamlres 2>/dev/null || { echo >&2 "ocp-ocamlres not installed. Please install: opam install ocp-ocamlres"; exit 1; }
hash camlp4 2>/dev/null || { echo >&2 "camlp4 not installed. Please install: opam install camlp4"; exit 1; }

./release.sh
rm bin/js_compiler.ml
make -j2 bin/jscmj.exe bin/jsgen.exe bin/js_compiler.ml
./bin/jsgen.exe --
./bin/jscmj.exe

ocamlc.opt -w -30-40 -no-check-prims -I bin  bin/config_whole_compiler.mli bin/config_whole_compiler.ml bin/js_compiler.mli bin/js_compiler.ml -o jsc.byte

## needs js_cmj_datasets, preload.js and amdjs to be update
# BS_PLAYGROUND=../../bucklescript-playground
# NEED PRESET env varialbe BS_PLAYGROUND
# node compiler_test.js in bucklescript-playground
js_of_ocaml  --toplevel +weak.js   ./polyfill.js jsc.byte -I ./stdlib -I ./others/ -I ./runtime/ \
--file lazy.cmi:/cmis/lazy.cmi --file js.cmi:/cmis/js.cmi --file js_unsafe.cmi:/cmis/js_unsafe.cmi \
--file js_re.cmi:/cmis/js_re.cmi --file js_array.cmi:/cmis/js_array.cmi --file js_null.cmi:/cmis/js_null.cmi \
--file js_undefined.cmi:/cmis/js_undefined.cmi --file js_internal.cmi:/cmis/js_internal.cmi --file js_types.cmi:/cmis/js_types.cmi \
--file js_null_undefined.cmi:/cmis/js_null_undefined.cmi --file js_dict.cmi:/cmis/js_dict.cmi --file js_string.cmi:/cmis/js_string.cmi\
-o $BS_PLAYGROUND/exports.js



rm -rf  $BS_PLAYGROUND/pre_load.js && cp ./pre_load.js $BS_PLAYGROUND
# TODO: build with amd first
cp ../lib/amdjs/*.js $BS_PLAYGROUND/stdlib

# note it is preferred to run ./release.sh && ./js.sh otherwise  amdjs is not really latest









