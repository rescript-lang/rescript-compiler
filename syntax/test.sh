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
