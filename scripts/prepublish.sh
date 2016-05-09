#!/bin/sh
set -e

cd jscomp && watchman watch-del . || true &&  cd ..
cd jscomp && git clean -dfx && cd ..
cd ocaml && git clean -dfx && git archive HEAD -o ../ocaml.tar.gz


