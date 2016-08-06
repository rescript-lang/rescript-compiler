#!/bin/sh
set -e
ocaml -w -40 gen_slots.ml > ocaml_stdlib_slots.ml
