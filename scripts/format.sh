#!/bin/bash

shopt -s extglob

dune build @fmt --auto-promote

files=$(find runtime tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*" ! -path "tests/syntax_tests/*" ! -path "tests/gentype_tests/typescript-react-example/node_modules/*")
./cli/rescript format $files
