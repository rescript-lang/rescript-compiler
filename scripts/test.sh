#!/bin/sh
set -e
node ./node_modules/mocha/bin/mocha './jscomp/test/**/*_test.js'  -R list "$@"
