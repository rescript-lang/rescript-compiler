#!/bin/sh
set -e
watchman watch-del .
make release

# TODO: if $OCAMLSCRIPT_RELEASE is current dir, should still work, but 
# don't remove it
# export OCAMLSCRIPT_RELEASE=

rm -rf $OCAMLSCRIPT_RELEASE/jscomp/bin/compiler.ml
cp bin/compiler.ml $OCAMLSCRIPT_RELEASE/jscomp/bin
cd $OCAMLSCRIPT_RELEASE/jscomp

git clean -dfx
ocamlopt.opt -g -linkall  -I +compiler-libs -I bin ocamlcommon.cmxa ocamlbytecomp.cmxa bin/compiler.mli bin/compiler.ml -o bin/osc

cd runtime
make all

cd ../stdlib
make all

cd ../lib 
make all

cd ../test
make all
