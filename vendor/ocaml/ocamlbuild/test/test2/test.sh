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
CMDOPTS="-- -help"
BUILD="$OCB toto.byte toto.native -no-skip -classic-display $@"
BUILD1="$BUILD $CMDOPTS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDOPTS"
rm -rf _build
cp vivi1.ml vivi.ml
$BUILD1
$BUILD2
cp vivi2.ml vivi.ml
$BUILD1
$BUILD2
cp vivi3.ml vivi.ml
$BUILD1
$BUILD2
