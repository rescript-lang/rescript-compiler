#!/bin/sh
set -e

ocamlopt.opt unix.cmxa ocamlcommon.cmxa -I +compiler-libs -I +unix ocaml_pack.ml -o ocaml_pack


