#!/bin/sh
set -e
watchman watch-del .
git clean -dfx
. ./env.sh
. ./build.sh
make release


git clean -dfx
make world-test
