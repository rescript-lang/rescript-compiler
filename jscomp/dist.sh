#!/bin/sh
set -e

cd dist && rm -rf * && cd ..
cd runtime && cp *.ml *.mli *.cmt  *.cmj *.cmi *.js ../dist && cd ..
cd stdlib && cp $(git ls-files) ../dist  && cd ..
cd dist && make all && rm *.ml *.mli  && cd ..
cd dist && rm *.ml *.mli && cd ..
# currently remove *.ml and *.mli for LICENSE issues
