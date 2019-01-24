#!/bin/sh

# usage: fix_index.sh <file>.idx

# This script works around a hyperref bug: hyperref does not handle
# quotes in \index arguments properly.
#
# Symptom:
# When \index{-pipe-pipe@\verb`("|"|)`} appears in your .tex, the hyperref
# package mangles it and produces this line in your .idx:
#   \indexentry{(-pipe-pipe)@\verb`("|hyperindexformat{\"}}{292}
# instead of the expected:
#   \indexentry{(-pipe-pipe)@\verb`("|"|)`|hyperpage}{292}
#
# This is because it fails to handle quoted characters correctly.
#
# The workaround:
# Look for the buggy line in the given .idx file and change it.

# Note: this bug will happen every time you have a | (pipe) character
# in an index entry (properly quoted with a " (double-quote) before it).
# We fix only the one case that appears in the OCaml documentation.
# We do not attempt a general solution because hyperref erases part
# of the argument, so we cannot recover the correct string from its
# output.

# Note 2013-06-19:
# The above was for the || operator in the stdlib's Pervasives module.
# Now we have the same problem with the |> operator that was added
# to the same module in commit 13739, hence the second special case.

usage(){
  echo "usage: fix_index.sh <file>.idx" >&2
  exit 2
}

case $# in
  1) ;;
  *) usage;;
esac

ed "$1" <<'EOF'
/-pipe-pipe/s/verb`("|hyperindexformat{\\"}/verb`("|"|)`|hyperpage/
/-pipe-gt/s/verb`("|hyperindexformat{\\>)`}/verb`("|>)`|hyperpage/
w
q
EOF

case $? in
  0) echo "fix_index.sh: fixed $1 successfully.";;
  *) echo "fix_index.sh: some error occurred."; exit 0;;
esac
