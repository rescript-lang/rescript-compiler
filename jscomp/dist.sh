#!/bin/sh
set -e

cd dist && rm -rf *.ml *.mli *.cm* *.js *.annot  && cd ..
cd runtime && cp *.ml *.mli *.cmt  *.cmj *.cmi *.js ../dist && cd ..
cd stdlib && cp $(git ls-files) ../dist  && cd ..
cd dist && make all && rm -f *.ml *.mli  && cd ..

# currently remove *.ml and *.mli for LICENSE issues
