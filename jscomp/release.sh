#!/bin/sh
set -e
watchman watch-del .
git clean -dfx . ../lib



. ./build.sh
make release




