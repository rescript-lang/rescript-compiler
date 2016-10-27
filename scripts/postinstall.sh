#!/bin/sh
set -e

export BS_RELEASE_BUILD=1
export PATH=$(pwd)/bin:$PATH

if [ $BS_TRAVIS_CI ]
then 
    cd jscomp && make travis-world-test && make install && cd ..
else 
    cd jscomp && make world && make install && cd ..
fi

# uninstall ocaml

node scripts/clean.js

