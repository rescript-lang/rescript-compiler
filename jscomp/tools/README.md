
# A tool to pack various ocaml files into a single file

To make it easy to ship our software

## Build the pack tool

```sh
ocamlopt.opt ocamlcommon.cmxa -I +compiler-libs ocaml_pack.ml -o ocaml_pack
```

## Bootstrap

```sh
./ocaml_pack depend.ml ocaml_extract.ml ocaml_extract.mli ocaml_pack_main.ml > ocaml_pack.ml
```
