#!/bin/sh
set -e

cd dist && git clean -dfx  && cd ..
cp bin/compiler.ml bin/compiler.mli dist/bin
cd dist && ocamlopt.opt -g -inline 100 -linkall  -I +compiler-libs -I bin ocamlcommon.cmxa ocamlbytecomp.cmxa bin/compiler.mli bin/compiler.ml -o bin/bsc && cd ..
cd runtime && cp *.ml *.mli *.cmt  *.cmj *.cmi *.js ../dist && cd ..
cd stdlib && cp $(git ls-files) ../dist  && cd ..
cd dist && make all && rm -f *.ml *.mli  && cd ..
cp test/*.ml test/*.mli dist/test 
cd dist && ./bin/bsc -I . -I test -c `ocamldep -sort -one-line -modules test/*.mli test/*.ml` && cd ..
cd dist && npm run test
# currently remove *.ml and *.mli for LICENSE issues
