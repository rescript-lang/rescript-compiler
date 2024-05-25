#!/bin/sh

./rewatch/target/release/rewatch --bsc-path "$(pwd)/bsc" clean

./rewatch/target/release/rewatch --bsc-path "$(pwd)/bsc" build
cp jscomp/others/*.js lib/js

contents="$(jq '."package-specs".module = "es6"' < rescript.json)" && echo "${contents}" > rescript.json

./rewatch/target/release/rewatch --bsc-path "$(pwd)/bsc" build
cp jscomp/others/*.js lib/es6

contents="$(jq '."package-specs".module = "commonjs"' < rescript.json)" && echo "${contents}" > rescript.json
