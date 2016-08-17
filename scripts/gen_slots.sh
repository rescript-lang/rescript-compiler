#!/bin/sh
set -e
cd "${0%/*}"
ocaml -w -40 ./gen_slots.ml > ../jscomp/ocaml_stdlib_slots.ml
