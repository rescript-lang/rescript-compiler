#!/bin/bash

shopt -s extglob

dune build @fmt --auto-promote

files=$(find runtime tests -type f \( -name "*.res" -o -name "*.resi" \) ! -name "syntaxErrors*")
./rescript format $files
