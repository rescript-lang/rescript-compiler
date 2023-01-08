#!/bin/bash

# Reanalyze (currently on syntax module only)
echo "Running reanalyze..."
reanalyze.exe -set-exit-code -all-cmt _build/default/res_syntax -suppress res_syntax/testrunner

# Check format (does not work on Windows)
case "$(uname -s)" in
  Darwin|Linux)
    echo "Checking code formatting..."
    if dune build @fmt; then
      printf "${successGreen}✅ Code formatting ok.${reset}\n"
    else
      printf "${warningYellow}⚠️ Code formatting failed.${reset}\n"
      exit 1
    fi
    ;;
esac
