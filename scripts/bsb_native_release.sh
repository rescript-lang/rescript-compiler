export CXXFLAGS="-mmacosx-version-min=10.10"
cd vendor/ocaml && make clean && make world.opt && make install && cd ../../
node scripts/install.js native
VERSION=$(cat package.json | sed -n -E 's/.*"version": "(.*)",/\1/p')
rm -rf bsb-native-osx-$VERSION.zip && zip -r bsb-native-osx-$VERSION.zip  lib/ vendor/ocaml/ocamlc.opt vendor/ocaml/ocamlopt.opt vendor/ocaml/lib/ocaml vendor/ocaml/bin/ocamlrun -x lib/bsb -x lib/bsc -x lib/bsrefmt -x vendor/ocaml/lib/ocaml/ocamlbuild/**/* -x vendor/ocaml/lib/ocaml/ocamldoc/**/* -x lib/bsb.cm* -x lib/bsb_helper.cm* -x lib/bsb.o -x lib/bsc.o -x lib/bsb_helper.o
