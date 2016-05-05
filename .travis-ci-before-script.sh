set -e

git submodule update --init --recursive

cd ocaml &&  ./configure -prefix $(dirname $(pwd))  -no-ocamldoc -no-ocamlbuild && make -j9 world.opt && make install && git clean -dfx && git checkout master && cd ..

export PATH=$(pwd)/bin:$PATH

cd jscomp && make world-test
