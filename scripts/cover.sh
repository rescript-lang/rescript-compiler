#!/bin/sh
set -e
node ./node_modules/istanbul/lib/cli.js cover --report html ./node_modules/.bin/_mocha --   jscomp/test/*test.js "$@"
