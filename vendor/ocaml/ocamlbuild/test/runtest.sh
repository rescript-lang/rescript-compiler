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
set -e
cd `dirname $0`

export OCB=$PWD/../../_build/ocamlbuild/ocamlbuild.native

myfiglet() {
  figlet $@ | sed 's/  *$//'
}

if figlet ""; then
  BANNER=myfiglet
else
  echo "Install figlet to have a better output, press enter to continue with echo"
  read
  BANNER=echo
fi

HERE=`pwd`

$BANNER Test2
./test2/test.sh $@
$BANNER Test3
./test3/test.sh $@
$BANNER Test4
./test4/test.sh $@
$BANNER Test5
./test5/test.sh $@
$BANNER Test6
./test6/test.sh $@
$BANNER Test7
./test7/test.sh $@
$BANNER Test8
./test8/test.sh $@
$BANNER Test9
./test9/test.sh $@
$BANNER Test10
./test10/test.sh $@
$BANNER Test11
./test11/test.sh $@
$BANNER Test12
./test12/test.sh $@
$BANNER Test Virtual Targets
./test_virtual/test.sh $@
