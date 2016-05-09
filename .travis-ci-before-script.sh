#!/bin/sh
set -e

git submodule update --init --recursive

export OCAMLPARAM='_,bin-annot=1' 
export OCAMLRUNPARAM=b

cd ocaml &&  ./configure -prefix $(dirname $(pwd))  -no-ocamldoc -no-ocamlbuild && make -j9 world.opt && make install  && cd ..

export PATH=$(pwd)/bin:$PATH

cd jscomp && make travis-world-test && make install
