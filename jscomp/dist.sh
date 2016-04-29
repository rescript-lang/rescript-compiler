#!/bin/sh
set -e

cd dist && git clean -dfx  && cd ..
cp bin/compiler.ml bin/compiler.mli dist/bin
cd runtime && cp *.ml *.mli *.cmt  *.cmj *.cmi *.js ../dist && cd ..
cd stdlib && cp $(git ls-files) ../dist  && cd ..
cd dist && make all && rm -f *.ml *.mli  && cd ..
cp test/*.ml dist/test && cd dist && bsc -I . -I test -c `ocamldep -sort -one-line -modules test/*.mli test/*.ml` && cd ..
# currently remove *.ml and *.mli for LICENSE issues
