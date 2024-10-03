#!/bin/bash

shopt -s extglob

dune build @fmt --auto-promote

files=$(find runtime tests/tests -type f \( -name "*.res" -o -name "*.resi" \))
./rescript format $files
