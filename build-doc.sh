#!/bin/sh
set -e
node process.js
cp -r dist/* $BUCKLESCRIPT_DOC
