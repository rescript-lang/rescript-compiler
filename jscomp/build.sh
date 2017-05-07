#!/bin/sh

set -e

export OCAMLPARAM='_,bin-annot=1,annot=1' 
export OCAMLRUNPARAM=b
export BS_DEBUG=true

export npm_package_name=bs-platform


echo "Building the compiler" > build.compile
make -j5 -r  check  2>>build.compile
# -k will keep going, due to failure, some cmi files not produced, false alarm
make -r -j2 bin/bsc.exe bin/bsb.exe 2>>build.compile
echo "Building finished" >> build.compile
echo "Building libs" >> build.compile
make libs 2>> build.compile
echo "Building finished" >> build.compile
# TODO: run make depend to snapshot .depend
# TODO: this quick test is buggy, 
# since a.ml maybe depend on another module 
# we can not just comment it, it will also produce jslambda


TARGET=a


echo ">>EACH FILE TESTING" >> build.compile
make -C test $TARGET.cmj 2>> ./build.compile

cat test/$TARGET.js >> ./build.compile
make -C test -j30 all 2>> ./build.compile
echo "<<Test finished" >> ./build.compile



# ocamlbuild  -cflags $OCAMLBUILD_CFLAGS -no-links js_generate_require.byte  -- 2>> ./build.compile

echo "........" >> ./build.compile


echo "Snapshot && update deps" >> ./build.compile

make -C test depend 2>>../build.compile
make -j7 depend snapshotml 2>> ./build.compile

echo "Updating dependencies in runtime"
cd runtime; make depend; cd ..

echo "Updating dependencies in others"
cd others; make depend; cd ..

echo "Done" >> ./build.compile
