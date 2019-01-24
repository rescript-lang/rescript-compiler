#!/bin/bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                         Christophe Troestler                           *
#*                                                                        *
#*   Copyright 2015 Christophe Troestler                                  *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BUILD_PID=0

function run {
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        if [ $BUILD_PID -ne 0 ] ; then
          kill -KILL $BUILD_PID 2>/dev/null
          wait $BUILD_PID 2>/dev/null
        fi
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

function set_configuration {
    cp config/m-nt.h byterun/caml/m.h
    cp config/s-nt.h byterun/caml/s.h

    FILE=$(pwd | cygpath -f - -m)/config/Makefile
    echo "Edit $FILE to set PREFIX=$2"
    sed -e "/PREFIX=/s|=.*|=$2|" \
        -e "/^ *CFLAGS *=/s/\r\?$/ $3\0/" \
         config/Makefile.$1 > config/Makefile
#    run "Content of $FILE" cat config/Makefile
}

APPVEYOR_BUILD_FOLDER=$(echo $APPVEYOR_BUILD_FOLDER| cygpath -f -)
# These directory names are specified here, because getting UTF-8 correctly
# through appveyor.yml -> Command Script -> Bash is quite painful...
OCAMLROOT=$(echo $PROGRAMFILES/Бактріан🐫| cygpath -f - -m)

# This must be kept in sync with appveyor_build.cmd
BUILD_PREFIX=🐫реализация

export PATH=$(echo $OCAMLROOT| cygpath -f -)/bin/flexdll:$PATH

case "$1" in
  install)
    mkdir -p "$OCAMLROOT/bin/flexdll"
    cd $APPVEYOR_BUILD_FOLDER/../flexdll
    # msvc64 objects need to be compiled with VS2015, so are copied later from
    # a source build.
    for f in flexdll.h flexlink.exe flexdll*_msvc.obj default*.manifest ; do
      cp $f "$OCAMLROOT/bin/flexdll/"
    done
    echo 'eval $($APPVEYOR_BUILD_FOLDER/tools/msvs-promote-path)' >> ~/.bash_profile
    ;;
  msvc32-only)
    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-msvc32

    set_configuration msvc "$OCAMLROOT-msvc32" -WX

    run "make world" make world
    run "make runtimeopt" make runtimeopt
    run "make -C otherlibs/systhreads libthreadsnat.lib" \
         make -C otherlibs/systhreads libthreadsnat.lib

    exit 0
    ;;
  test)
    FULL_BUILD_PREFIX=$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX
    run "ocamlc.opt -version" $FULL_BUILD_PREFIX-msvc64/ocamlc.opt -version
    run "test msvc64" make -C $FULL_BUILD_PREFIX-msvc64 tests
    run "test mingw32" make -C $FULL_BUILD_PREFIX-mingw32 tests
    run "install msvc64" make -C $FULL_BUILD_PREFIX-msvc64 install
    run "install mingw32" make -C $FULL_BUILD_PREFIX-mingw32 install
    ;;
  *)
    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-msvc64

    tar -xzf $APPVEYOR_BUILD_FOLDER/flexdll.tar.gz
    cd flexdll-$FLEXDLL_VERSION
    make MSVC_DETECT=0 CHAINS=msvc64 support
    cp flexdll*_msvc64.obj "$OCAMLROOT/bin/flexdll/"
    cd ..

    set_configuration msvc64 "$OCAMLROOT" -WX

    cd ../$BUILD_PREFIX-mingw32

    set_configuration mingw "$OCAMLROOT-mingw32" -Werror

    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-msvc64

    export TERM=ansi
    script --quiet --return --command "make -C ../$BUILD_PREFIX-mingw32 flexdll world.opt" ../$BUILD_PREFIX-mingw32/build.log >/dev/null 2>/dev/null &
    BUILD_PID=$!

    run "make world" make world
    run "make bootstrap" make bootstrap
    run "make opt" make opt
    run "make opt.opt" make opt.opt

    set +e

    # For an explanation of the sed command, see https://github.com/appveyor/ci/issues/1824
    tail --pid=$BUILD_PID -n +1 -f ../$BUILD_PREFIX-mingw32/build.log | sed -e 's/\d027\[K//g' -e 's/\d027\[m/\d027[0m/g' -e 's/\d027\[01\([m;]\)/\d027[1\1/g' &
    TAIL_PID=$!
    wait $BUILD_PID
    STATUS=$?
    wait $TAIL_PID
    exit $STATUS
    ;;
esac
