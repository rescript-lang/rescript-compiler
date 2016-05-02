#!/bin/sh

## build bundler first
./pack.sh
## bootstrap, if successful, there should be no diff with regard to `ocaml_pack.ml
./repack.sh
