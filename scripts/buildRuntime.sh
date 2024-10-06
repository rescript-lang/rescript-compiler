#!/bin/bash
set -e
shopt -s extglob

rm -f lib/es6/*.js lib/js/*.js lib/ocaml/*
mkdir -p lib/es6 lib/js lib/ocaml
mkdir -p runtime/lib/es6 runtime/lib/js

(cd runtime && ../cli/rescript build)

cp runtime/lib/es6/*.js lib/es6
cp runtime/lib/js/*.js lib/js
cp runtime/lib/bs/!(belt_internal*).cmi lib/ocaml/
cp runtime/lib/bs/*.@(cmj|cmt|cmti) lib/ocaml/
cp runtime/*.@(res|resi) lib/ocaml/
