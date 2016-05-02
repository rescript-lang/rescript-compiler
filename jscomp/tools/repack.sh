#!/bin/sh
set -e
./ocaml_pack tools.mllib > ocaml_pack.ml
ocamlc.opt -I +compiler-libs -c -i ocaml_pack.ml
