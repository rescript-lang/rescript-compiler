#!/bin/bash

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

case "$(uname -s)" in
  Darwin|Linux)
    echo "Checking code formatting..."
    if opam exec -- dune build @fmt; then
      printf "${successGreen}✅ OCaml code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ OCaml code formatting issues found.${reset}\n"
      exit 1
    fi
    ;;
  *)
    # Does not work on Windows
    echo "OCaml code formatting check skipped for this platform."
esac
