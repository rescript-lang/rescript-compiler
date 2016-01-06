#!/bin/sh
set -e 
eslint -f unix . > ./err.compile
