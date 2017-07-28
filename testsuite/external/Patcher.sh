#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#           Damien Doligez, projet Gallium, INRIA Rocquencourt          #
#                                                                       #
#   Copyright 2012 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# usage:
# Patcher.sh <directory> [<patchfile>]

if [ -f "$1.patch" ]; then
  echo "patch -d $1 -p1 < $1.patch"
  patch -d $1 -p1 < "$1.patch"
fi

if [ -f "$1-$VERSION.patch" ]; then
  echo "patch -d $1 -p1 < $1-$VERSION.patch"
  patch -d $1 -p1 < "$1-$VERSION.patch"
fi

if [ -f "$2" ]; then
  echo "patch -d $1 -l -p0 < $2"
  patch -d $1 -l -p0 < "$2" || exit 0
fi
