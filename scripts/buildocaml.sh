#!/bin/sh
set -e


export OCAMLPARAM='_,bin-annot=1'
export OCAMLRUNPARAM=b

if [ $BS_TRAVIS_CI ]
then
    git submodule update --init --recursive
    OCAML=ocaml
else
    OCAML=ocaml_src
    if [  -f "ocaml.tar.gz" ]
    then
        echo "ocaml.tar.gz already exists"
        rm -rf $OCAML
        mkdir -p $OCAML
        tar -zxvf ./ocaml.tar.gz -C $OCAML
    else
        echo "creating ocaml from github"
        if [ -d "$OCAML" ]
        then
            cd $OCAML && git reset --hard && git pull --depth 1 && cd -
        else
            git clone -b master --depth 1 https://github.com/bloomberg/ocaml $OCAML
        fi
    fi
fi

cd $OCAML &&  ./configure -prefix $(dirname $(pwd))  -no-ocamldoc -no-ocamlbuild -no-shared-libs -no-curses -no-graph -no-pthread -no-debugger  && make -j9 world.opt && make install  && cd ..
