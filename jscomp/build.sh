#!/bin/sh

set -e

export OCAMLPARAM='_,bin-annot=1,annot=1' 
export OCAMLRUNPARAM=b
# No support for -optflags
export OCAMLBUILD_CFLAGS=-g,-w,-40-30,-warn-error,+a-40-30,-keep-locs,-I,+compiler-libs
export npm_package_name=bs-platform


## Only make sense for dev 
make -j4 js_map.ml js_fold.ml sexp_lexer.ml bs_json.ml 2>> build.compile
# lam_map.ml lam_fold.ml
## Disable it when make a release 

ocamlbuild -no-links -cflags $OCAMLBUILD_CFLAGS foo.otarget > build.compile

make -r bin/bsc.exe
# TODO: this quick test is buggy, 
# since a.ml maybe depend on another module 
# we can not just comment it, it will also produce jslambda

# echo "LINKING FINISHED\n" >> build.compile
# make -S  -j1 quicktest 2>> build.compile
# cat stdlib/a.js >> ./build.compile

echo "Making stdlib cmis" >> build.compile
cd ./stdlib ; make allcmis; cd ..
echo "Making stdlib finished" >> build.compile

echo "Making runtime" >> build.compile
cd ./runtime; make all 2>> ../build.compile ; make depend;  cd ..
echo "Making runtime Finished" >> build.compile

echo "Remaking standard library" >> build.compile
cd ./stdlib; ./build.sh ; cd ../
echo "Remaking standard library Finished" >> build.compile

echo "Making others"
cd others && make all 2>> ../build.compile ; make depend; cd ..
echo "Making others finished"

TARGET=a

# Building files in stdlib
echo ">>EACH FILE TESTING" >> build.compile
cd ./test/
# ./build.sh 2>> ../build.compile
make $TARGET.cmj 2>> ../build.compile

cat $TARGET.js >> ../build.compile
make -j30 all 2>>../build.compile
echo "<<Test finished" >> ../build.compile
make depend 2>>../build.compile
cd ..


ocamlbuild  -cflags $OCAMLBUILD_CFLAGS -no-links js_generate_require.byte  -- 2>> ./build.compile

echo "........" >> ./build.compile

# npm run cover&

echo "Done"

echo "Make pack tools" >> ./build.compile


# make snapshot
# generate new js_cmj_datasets
make snapshotml 2>> ./build.compile
echo "Done" >> ./build.compile
