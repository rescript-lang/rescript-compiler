#!/bin/sh
set -e

if [ $BS_TRAVIS_CI ]
then 
    git submodule update --init --recursive
    OCAML=ocaml
else 
    OCAML=ocaml_src
    rm -rf $OCAML
    mkdir -p $OCAML
    tar -zxvf ./ocaml.tar.gz -C $OCAML

fi 


export OCAMLPARAM='_,bin-annot=1' 
export OCAMLRUNPARAM=b

# we encoruage people to install from opam 
# for our own local installation 
# it can be minimal
cd $OCAML &&  ./configure -prefix $(dirname $(pwd))  -no-ocamldoc -no-ocamlbuild -no-shared-libs -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install  && cd ..


export PATH=$(pwd)/bin:$PATH

if [ $BS_TRAVIS_CI ]
then 
    cd jscomp && make travis-world-test && make install
else 
    cd jscomp && make world && make install
fi
