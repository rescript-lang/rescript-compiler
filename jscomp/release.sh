#!/bin/sh
set -e
watchman watch-del .
git clean -dfx . ../lib
make clean
make -j9 check
make force-snapshotml
make -j1 world





