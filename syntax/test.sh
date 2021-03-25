#!/bin/zsh

setopt extendedglob

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

# parsing
for file in tests/parsing/{errors,infiniteLoops,recovery}/**/*.(res|resi); do
  lib/rescript.exe -recover -print ml $file &> $(exp $file) &
done
for file in tests/parsing/{grammar,other}/**/*.(res|resi); do
  lib/rescript.exe -print ml $file &> $(exp $file) &
done

# printing
for file in tests/{printer,conversion}/**/*.(res|resi|ml|mli); do
  lib/rescript.exe $file &> $(exp $file) &
done
for file in tests/{printer,conversion}/**/*.re; do
  lib/refmt.exe --parse re --print binary $file | lib/rescript.exe -parse reasonBinary &> $(exp $file) &
done
for file in tests/{printer,conversion}/**/*.rei; do
  lib/refmt.exe --parse re --print binary --interface true $file | lib/rescript.exe -parse reasonBinary -interface &> $(exp $file) &
done

# printing with ppx
for file in tests/ppx/react/*.(res|resi); do
  lib/rescript.exe -ppx jsx $file &> $(exp $file) &
done

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

git diff --quiet tests/
if [[ "$?" = 0 ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n\n"
  git ls-files --modified tests/
  printf $reset
  exit 1
fi

# roundtrip tests
if [[ $ROUNDTRIP_TEST = 1 ]]; then
  echo "Running roundtrip tests…"
  mkdir -p temp
  roundtripTestsResult="temp/result.txt"
  touch $roundtripTestsResult

  function run {
    file=$1
    class=$2
    if [[ $3 = true ]]; then
      refmtInterfaceArg=--interface true
      rescriptInterfaceArg=-interface
    fi
    mkdir -p temp/$(dirname $file)
    reasonBinaryFile=temp/$file.reasonBinary
    lib/refmt.exe --parse $class --print binary $refmtInterfaceArg $file > $reasonBinaryFile
    sexpAst=temp/$file.sexp
    lib/rescript.exe -parse reasonBinary -print sexp $rescriptInterfaceArg $reasonBinaryFile > $sexpAst
    rescript=temp/$file.res
    lib/rescript.exe -parse reasonBinary $rescriptInterfaceArg $reasonBinaryFile > $rescript
    rescriptSexpAst=temp/$file.ressexp
    lib/rescript.exe -print sexp $rescriptInterfaceArg $rescript > $rescriptSexpAst
    rescript2=temp/$file.2.res
    lib/rescript.exe $rescriptInterfaceArg $rescript > $rescript2

    diff --unified $sexpAst $rescriptSexpAst
    if [[ "$?" = 1 ]]; then
      echo 1 > $roundtripTestsResult
    fi
    diff --unified $rescript $rescript2
    if [[ "$?" = 1 ]]; then
      echo 1 > $roundtripTestsResult
    fi
  }

  for file in tests/idempotency/**/*.re; do
    run $file re false &
  done
  for file in tests/idempotency/**/*.rei; do
    run $file re true &
  done
  for file in tests/idempotency/**/*.ml; do
    run $file ml false &
  done
  for file in tests/idempotency/**/*.mli; do
    run $file ml true &
  done

  wait

  result=$(cat $roundtripTestsResult)
  rm -r temp/

  if [[ $result = "1" ]]; then
    printf "${warningYellow}⚠️ Roundtrip tests failed.${reset}\n"
    exit 1
  else
    printf "${successGreen}✅ Roundtrip tests succeeded.${reset}\n"
  fi

fi
