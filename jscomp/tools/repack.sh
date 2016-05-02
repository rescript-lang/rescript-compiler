#!/bin/sh
set -e
./ocaml_pack tools.mllib > ocaml_pack.ml
ocamlc.opt -w -40 -I +compiler-libs -c -i ocaml_pack.ml
