#!/bin/sh
set -e
watchman watch-del .
git clean -dfx . ../lib
make clean
make themes
make -j9 check
BS_DEUBG=false make force-snapshotml
make -j1 world





