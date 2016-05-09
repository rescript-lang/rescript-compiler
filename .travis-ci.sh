#!/bin/sh
set -e

if [ $BS_TRAVIS_CI ]
then 
    git submodule update --init --recursive
fi 

export OCAMLPARAM='_,bin-annot=1' 
export OCAMLRUNPARAM=b

# we encoruage people to install from opam 
# for our own local installation 
# it can be minimal
cd ocaml &&  ./configure -prefix $(dirname $(pwd))  -no-ocamldoc -no-ocamlbuild -no-shared-libs -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install  && cd ..

# we should not rely on git
# if [ ! $BS_TRAVIS_CI ]
# then 
#     cd ocaml && git clean -dfx && cd ..
# fi

export PATH=$(pwd)/bin:$PATH

cd jscomp && make travis-world-test && make install
