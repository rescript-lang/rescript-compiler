#!/bin/sh


../bin/ocaml_pack tools.mllib > ./ocaml_pack0.ml
ocamlopt.opt -w -40 -I +compiler-libs  ./ocaml_pack0.ml -o ./ocaml_pack
./ocaml_pack tools.mllib > ./ocaml_pack1.ml

if ! diff -q ./ocaml_pack0.ml ./ocaml_pack1.ml > /dev/null  2>&1; then
    echo "they differ"
else 
    echo "fixpoint"
fi
