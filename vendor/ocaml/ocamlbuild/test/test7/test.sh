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
set -e
set -x
CMDOPTS="" # -- command args
BUILD="$OCB bbcc.cma main.byte bbcc.cmxa main.native -no-skip -classic-display $@"
BUILD1="$BUILD $CMDARGS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDARGS"
rm -rf _build
cp bb1.ml bb.ml
$BUILD1
$BUILD2
cp bb2.ml bb.ml
$BUILD1 -verbose 0
$BUILD2
cp bb3.ml bb.ml
$BUILD1 -verbose 0
$BUILD2
