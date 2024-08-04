#!/bin/bash

shopt -s extglob

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

case "$(uname -s)" in
  Darwin|Linux)
    echo "Checking OCaml code formatting..."
    if opam exec -- dune build @fmt; then
      printf "${successGreen}✅ OCaml code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ OCaml code formatting issues found.${reset}\n"
      exit 1
    fi

    echo "Checking ReScript code formatting..."
    if ./rescript format -check jscomp/@(others|runtime)/*.@(res|resi); then
      printf "${successGreen}✅ ReScript code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ ReScript code formatting issues found.${reset}\n"
      exit 1
    fi
    ;;
  *)
    # Does not work on Windows
    echo "Code formatting checks skipped for this platform."
esac

