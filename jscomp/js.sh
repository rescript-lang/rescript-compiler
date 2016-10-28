#!/bin/sh
set -e 

## needs js_cmj_datasets, preload.js and amdjs to be update
./bin/jsgen.exe --
rm -rf  $DOCS_DIR/pre_load.js && cp ./pre_load.js $DOCS_DIR
# TODO: build with amd first 
cp ../lib/amdjs/*.js $DOCS_DIR/stdlib

# note it is preferred to run ./release.sh && ./js.sh otherwise  amdjs is not really latest









