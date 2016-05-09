#!/bin/sh

set -e
. ./env.sh

## Only make sense for dev 
make js_map.ml js_fold.ml  >> build.compile
# lam_map.ml lam_fold.ml
## Disable it when make a release 

ocamlbuild  -cflags $OCAMLBUILD_CFLAGS compiler.cmxa > build.compile

make -r bin/bsc
# TODO: this quick test is buggy, 
# since a.ml maybe depend on another module 
# we can not just comment it, it will also produce jslambda

# echo "LINKING FINISHED\n" >> build.compile
# make -S  -j1 quicktest 2>> build.compile
# cat stdlib/a.js >> ./build.compile

echo "Making runtime" >> build.compile

cd ./runtime; make all 2>> ../build.compile ; make depend;  cd ..
echo "Making runtime Finished" >> build.compile

echo "Remaking standard library" >> build.compile
cd ./stdlib; ./build.sh ; cd ../
echo "Remaking standard library Finished" >> build.compile

echo "Remaking thirdparty library" >> build.compile
cd ./lib; ./build.sh ; cd ../
echo "Remaking thirdparty library Finished" >> build.compile

TARGET=a

# Building files in stdlib
echo ">>EACH FILE TESTING" >> build.compile
cd ./test/
# ./build.sh 2>> ../build.compile
make $TARGET.cmj 2>> ../build.compile

cat ../../lib/js/test/$TARGET.js >> ../build.compile
make -j30 all 2>>../build.compile
make depend 2>>../build.compile
echo "<<Test finished" >> ../build.compile
cd ..


ocamlbuild  -cflags $OCAMLBUILD_CFLAGS -no-links js_generate_require.byte  -- 2>> ./build.compile

echo "........" >> ./build.compile

# npm run cover&

echo "Done"
