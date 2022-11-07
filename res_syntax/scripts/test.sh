#!/bin/bash

# Note:
# 1. This was converted from zsh to bash because zsh is not available on Linux and Windows Github action runners.
# 2. macOS still has bash 3 and therefore no globstar ("**") support.
#    Therefore we need to use find + temp files for the file lists.

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && wait
}

rm -rf temp
mkdir temp

# parsing
find tests/parsing/{errors,infiniteLoops,recovery} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  rescript -recover -print ml $file &> $(exp $file) & maybeWait
done <temp/files.txt
find tests/parsing/{grammar,other} -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  rescript -print ml $file &> $(exp $file) & maybeWait
done <temp/files.txt

# printing
find tests/{printer,conversion} -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
while read file; do
  rescript $file &> $(exp $file) & maybeWait
done <temp/files.txt

# printing with ppx
find tests/ppx/react -name "*.res" -o -name "*.resi" >temp/files.txt
while read file; do
  rescript -jsx-version 4 -jsx-mode "automatic" $file &> $(exp $file) & maybeWait
done <temp/files.txt

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

git diff --ignore-cr-at-eol $(find tests -name expected) >temp/diff.txt
diff=$(cat temp/diff.txt)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  rm -r temp/
  exit 1
fi

# roundtrip tests
if [[ $ROUNDTRIP_TEST = 1 ]]; then
  echo "Running roundtrip tests…"
  roundtripTestsResult="temp/result.txt"
  touch $roundtripTestsResult

  find tests/{idempotency,printer} -name "*.res" -o -name "*.resi" -o -name "*.ml" -o -name "*.mli" >temp/files.txt
  while read file; do {
    mkdir -p temp/$(dirname $file)
    sexpAst1=temp/$file.sexp
    sexpAst2=temp/$file.2.sexp
    rescript1=temp/$file.res
    rescript2=temp/$file.2.res

    case $file in
      *.ml   ) class="ml" ; resIntf=""         ;;
      *.mli  ) class="ml" ; resIntf=-interface ;;
      *.res  ) class="res"; resIntf=""         ;;
      *.resi ) class="res"; resIntf=-interface ;;
    esac

    rescript $resIntf -parse $class -print sexp $file > $sexpAst1
    rescript $resIntf -parse $class -print res $file > $rescript1

    rescript $resIntf -print sexp $rescript1 > $sexpAst2
    rescript $resIntf -print res $rescript1 > $rescript2

    diff --unified $sexpAst1 $sexpAst2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
    diff --unified $rescript1 $rescript2
    [[ "$?" = 1 ]] && echo 1 > $roundtripTestsResult
  } & maybeWait
  done <temp/files.txt

  wait

  result=$(cat $roundtripTestsResult)

  if [[ $result = "1" ]]; then
    printf "${warningYellow}⚠️ Roundtrip tests failed.${reset}\n"
    exit 1
  else
    printf "${successGreen}✅ Roundtrip tests succeeded.${reset}\n"
  fi

fi

rm -r temp/

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
