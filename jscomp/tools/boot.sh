#!/bin/sh

## build bundler first
ocamlopt.opt unix.cmxa ocamlcommon.cmxa -I +compiler-libs -I +unix ocaml_pack.ml -o ocaml_pack

## bootstrap, if successful, there should be no diff with regard to `ocaml_pack.ml
./ocaml_pack tools.mllib > ocaml_pack.ml
