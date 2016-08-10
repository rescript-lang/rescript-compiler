#!/bin/sh

set -e


if [ ! -f ./ocamlpack ]; then 
    cp ../_build/ocamlpack ./
fi 

./ocamlpack tools.mllib > ./ocamlpack.ml
ocamlopt.opt -w -40 -I +compiler-libs  unix.cmxa ocamlcommon.cmxa ./ocamlpack.ml -o ./ocamlpack

# if linking succeed should be fine to copy
cp ./ocamlpack.ml ../bin/ocamlpack.ml

