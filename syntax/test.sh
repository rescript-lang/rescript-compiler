#!/bin/zsh

setopt extendedglob

function run {
  if [ $1 = true ]; then recover="-recover" else recover="" fi
  for file in ${@:2}; do
    expected=$(dirname $file)/expected/$(basename $file).txt
    ./lib/rescript.exe $recover -print ml $file &> $expected &
  done
}

run true ./tests/parsing/errors/**/*.(res|resi)
run false ./tests/parsing/grammar/**/*.(res|resi)
run true ./tests/parsing/infiniteLoops/*.(res|resi)
run true ./tests/parsing/recovery/**/*.(res|resi)

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

for file in ./tests/printer/**/*.(res|resi); do
  ./lib/rescript.exe $file &> $(exp $file) &
done
for file in ./tests/printer/**/*.(ml|mli); do
  ./lib/rescript.exe -parse ml $file &> $(exp $file) &
done
for file in tests/printer/**/*.re; do
  lib/refmt.exe --parse re --print binary $file | lib/rescript.exe -parse reasonBinary &> $(exp $file) &
done
for file in tests/printer/**/*.rei; do
  lib/refmt.exe --parse re --print binary --interface true $file | lib/rescript.exe -parse reasonBinary -interface &> $(exp $file) &
done


wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

git diff --quiet ./tests/parsing/
if [[ "$?" -eq 0 ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/parsing/! Did you break a test?\n\n"
  git ls-files --modified ./tests/parsing
  printf $reset
fi
