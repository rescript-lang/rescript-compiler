#!/bin/sh
set -e


# export OCAMLPARAM='_,bin-annot=1'
# export OCAMLRUNPARAM=b

cd vendor/ocaml
./configure -prefix `pwd`  -no-ocamldoc -no-ocamlbuild -no-shared-libs -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install  && cd ..
