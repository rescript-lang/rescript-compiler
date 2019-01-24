#!/bin/bash

set -e

$OCAMLOPT -c -afl-instrument test.ml
$OCAMLOPT -afl-inst-ratio 0 test.cmx harness.ml -o test

NTESTS=`./test len`
failures=''
echo "running $NTESTS tests..."
for t in `seq 1 $NTESTS`; do
  printf "%14s: " `./test name $t`
  # when run twice, the instrumentation output should double
  afl-showmap -q -o output-1 -- ./test 1 $t
  afl-showmap -q -o output-2 -- ./test 2 $t
  # see afl-showmap.c for what the numbers mean
  cat output-1 | sed '
    s/:6/:7/; s/:5/:6/;
    s/:4/:5/; s/:3/:4/;
    s/:2/:4/; s/:1/:2/;
  ' > output-2-predicted
  if cmp -s output-2-predicted output-2; then
    echo "passed."
  else
    echo "failed:"
    paste output-2 output-1
    failures=1
  fi
done

if [ -z "$failures" ]; then echo "all tests passed"; else exit 1; fi

rm -f {test,harness}.{cmi,cmx,o} test output-{1,2,2-predicted}
