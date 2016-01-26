#!/bin/sh
set -e

# Not needed only for bootstrapping
# ocaml_pack ../js-lambda/tools/depend.ml ../js-lambda/tools/depend.mli ocaml_extract.ml ocaml_extract.mli ocaml_pack_main.ml > ocaml_pack.ml
ocamlopt.opt unix.cmxa ocamlcommon.cmxa -I +compiler-libs -I +unix ocaml_pack.ml -o ocaml_pack


