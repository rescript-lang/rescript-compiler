#!/bin/sh

set -e


if [ ! -f ./ocamlpack ]; then 
    cp ../_build/ocamlpack ./
fi 

./ocamlpack tools.mllib > ./ocamlpack0.ml
ocamlopt.opt -w -40 -I +compiler-libs  unix.cmxa ocamlcommon.cmxa ./ocamlpack0.ml -o ./ocamlpack
./ocamlpack tools.mllib > ./ocamlpack1.ml

if ! diff -q ./ocamlpack0.ml ./ocamlpack1.ml > /dev/null  2>&1; then
    echo "they differ"
else 
    echo "fixpoint"
    cp ./ocamlpack0.ml ../bin/ocamlpack.ml
fi
