#!/bin/bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*              Anil Madhavapeddy, OCaml Labs                             *
#*                                                                        *
#*   Copyright 2014 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

PREFIX=~/local

MAKE=make SHELL=dash

# TRAVIS_COMMIT_RANGE has the form   <commit1>...<commit2>
# TRAVIS_CUR_HEAD is <commit1>
# TRAVIS_PR_HEAD is <commit2>
#
# The following diagram illustrates the relationship between
# the commits:
#
#      (trunk)         (pr branch)
#  TRAVIS_CUR_HEAD   TRAVIS_PR_HEAD
#        |            /
#       ...         ...
#        |          /
#  TRAVIS_MERGE_BASE
#
echo TRAVIS_COMMIT_RANGE=$TRAVIS_COMMIT_RANGE
TRAVIS_CUR_HEAD=${TRAVIS_COMMIT_RANGE%%...*}
TRAVIS_PR_HEAD=${TRAVIS_COMMIT_RANGE##*...}
case $TRAVIS_EVENT_TYPE in
   # If this is not a pull request then TRAVIS_COMMIT_RANGE may be empty.
   pull_request)
     TRAVIS_MERGE_BASE=$(git merge-base $TRAVIS_CUR_HEAD $TRAVIS_PR_HEAD);;
esac

BuildAndTest () {
  mkdir -p $PREFIX
  cat<<EOF
------------------------------------------------------------------------
This test builds the OCaml compiler distribution with your pull request
and runs its testsuite.

Failing to build the compiler distribution, or testsuite failures are
critical errors that must be understood and fixed before your pull
request can be merged.
------------------------------------------------------------------------
EOF
  case $XARCH in
  x64)
    ./configure --prefix $PREFIX -with-debug-runtime \
      -with-instrumented-runtime $CONFIG_ARG
    ;;
  i386)
    ./configure --prefix $PREFIX -with-debug-runtime \
      -with-instrumented-runtime $CONFIG_ARG \
      -host i686-pc-linux-gnu
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac

  export PATH=$PREFIX/bin:$PATH
  $MAKE world.opt
  $MAKE ocamlnat
  (cd testsuite && $MAKE all)
  [ $XARCH =  "i386" ] ||  (cd testsuite && $MAKE USE_RUNTIME="d" all)
  $MAKE install
  $MAKE manual-pregen
  # check_all_arches checks tries to compile all backends in place,
  # we would need to redo (small parts of) world.opt afterwards to
  # use the compiler again
  $MAKE check_all_arches
}

CheckChangesModified () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that the Changes file has been modified by the pull
request. Most contributions should come with a message in the Changes
file, as described in our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#changelog

Some very minor changes (typo fixes for example) may not need
a Changes entry. In this case, you may explicitly disable this test by
adding the code word "No change entry needed" (on a single line) to
a commit message of the PR, or using the "no-change-entry-needed" label
on the github pull request.
------------------------------------------------------------------------
EOF
  # check that Changes has been modified
  git diff $TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD --name-only --exit-code Changes \
    > /dev/null && CheckNoChangesMessage || echo pass
}

CheckNoChangesMessage () {
  API_URL=https://api.github.com/repos/$TRAVIS_REPO_SLUG/issues/$TRAVIS_PULL_REQUEST/labels
  if test -n "$(git log --grep="[Nn]o [Cc]hange.* needed" --max-count=1 \
    ${TRAVIS_MERGE_BASE}..${TRAVIS_PR_HEAD})"
  then echo pass
  elif test -n "$(curl $API_URL | grep 'no-change-entry-needed')"
  then echo pass
  else exit 1
  fi
}

CheckTestsuiteModified () {
  cat<<EOF
------------------------------------------------------------------------
This test checks that the OCaml testsuite has been modified by the
pull request. Any new feature should come with tests, bugs should come
with regression tests, and generally any change in behavior that can
be exercised by a test should come with a test or modify and existing
test. See our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#test-you-must

Modifications that result in no change in observable behavior
(documentation contributions for example) can hardly be tested, in
which case it is acceptable for this test to fail.

Note: the heuristic used by this test is extremely fragile; passing it
does *not* imply that your change is appropriately tested.
------------------------------------------------------------------------
EOF
  # check that at least a file in testsuite/ has been modified
  git diff $TRAVIS_MERGE_BASE..$TRAVIS_PR_HEAD --name-only --exit-code \
    testsuite > /dev/null && exit 1 || echo pass
}

case $CI_KIND in
build) BuildAndTest;;
changes)
    case $TRAVIS_EVENT_TYPE in
        pull_request) CheckChangesModified;;
    esac;;
tests)
    case $TRAVIS_EVENT_TYPE in
        pull_request) CheckTestsuiteModified;;
    esac;;
*) echo unknown CI kind
   exit 1
   ;;
esac
