#!/usr/bin/env bash

set -e

# Node 16 doesn't work, see https://forums.freebsd.org/threads/nodejs-and-libcrypto-so-wrong-version-after-upgrade.82471/
sudo pkg install -y npm-node14 python gcc

npm i rescript*.tgz

npx rescript -h
npx rescript build
cat src/Test.bs.js

# Cleanup
rm -rf node_modules
