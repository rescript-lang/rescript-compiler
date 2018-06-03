#!/bin/sh

# dev small utils
# hot replace global bsb.exe for quick testing
npm_prefix=`npm prefix -g`
cp ../lib/bsc.exe $npm_prefix/lib/node_modules/bs-platform/lib/
