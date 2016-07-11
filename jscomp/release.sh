#!/bin/sh
set -e
watchman watch-del .
git clean -dfx . ../lib

export BS_RELEASE_BUILD=1

. ./build.sh
# make release


# git clean -dfx
# make world-test
