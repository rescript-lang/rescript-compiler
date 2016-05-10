#!/bin/sh
set -e
watchman watch-del .
git clean -dfx

. ./build.sh
make release


git clean -dfx
make world-test
