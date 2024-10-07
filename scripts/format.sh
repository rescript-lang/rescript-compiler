#!/bin/bash

shopt -s extglob

dune build @fmt --root compiler --auto-promote

files=$(find runtime tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*")
./cli/rescript format $files
