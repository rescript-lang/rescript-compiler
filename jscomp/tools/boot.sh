#!/bin/sh

set -e


if [ ! -f ./ocaml_pack ]; then 
    cp ../bin/ocaml_pack ./
fi 

./ocaml_pack tools.mllib > ./ocaml_pack0.ml
ocamlopt.opt -w -40 -I +compiler-libs  unix.cmxa ocamlcommon.cmxa ./ocaml_pack0.ml -o ./ocaml_pack
./ocaml_pack tools.mllib > ./ocaml_pack1.ml

if ! diff -q ./ocaml_pack0.ml ./ocaml_pack1.ml > /dev/null  2>&1; then
    echo "they differ"
else 
    echo "fixpoint"
    cp ./ocaml_pack0.ml ../bin/ocaml_pack.ml
fi
