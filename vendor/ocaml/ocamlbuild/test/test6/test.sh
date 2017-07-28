#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#   Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt  #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

#!/bin/sh
cd `dirname $0`
set -x
rm -rf _build
CMDOPTS="" # -- command args
BUILD="$OCB -no-skip main.byte -classic-display $@"
BUILD1="$BUILD $CMDOPTS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDOPTS"
cp b.mli.v1 b.mli
cp d.mli.v1 d.mli
$BUILD1
$BUILD2
cp b.mli.v2 b.mli
cp d.mli.v2 d.mli
$BUILD1
cp b.mli.v1 b.mli
if $BUILD1; then
  if $BUILD2; then
    echo PASS
  else
    echo "FAIL (-nothing-should-be-rebuilt)"
  fi
else
  echo FAIL
fi
