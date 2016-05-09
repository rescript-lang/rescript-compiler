#!/bin/sh
set -e

# cd jscomp && watchman watch-del . || true &&  cd ..
# cd jscomp && git clean -dfx && cd ..

# have no idea why the tar.gz is not correct
cd ocaml && git clean -dfx && git archive HEAD -o ../ocaml.tar.gz && cd ..


