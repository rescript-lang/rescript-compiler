#!/bin/sh
set -e
make -r -j30 all 2>>../build.compile
make depend
