#!/bin/sh
set -e
export LP=../ocaml/bin
export OCAMLPARAM='_,bin-annot=1,annot=1,g=1' 
export OCAMLRUNPARAM=b
# export OCAML_RAW_JS=1
export OCAML_RAW_LAMBDA=1
export OCAMLBUILD_CFLAGS=-g,-w,-40-30,-warn-error,+a-40-30,-keep-locs,-I,+compiler-libs
