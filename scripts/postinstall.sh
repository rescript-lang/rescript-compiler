#!/bin/sh
set -e


export PATH=$(pwd)/bin:$PATH

if [ $BS_TRAVIS_CI ]
then 
    cd jscomp && make travis-world-test && make install && cd ..
else 
    cd jscomp && make world && make install && cd ..
fi

# uninstall ocaml

node scripts/clean.js

