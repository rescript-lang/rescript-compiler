#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#          Damien Doligez, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2003 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  As an exception to the       #
#   licensing rules of OCaml, this file is freely redistributable,      #
#   modified or not, without constraints.                               #
#                                                                       #
#########################################################################

# This script extracts the components from an OCaml version number
# and provides them as C defines:
# OCAML_VERSION_MAJOR: the major version number
# OCAML_VERSION_MAJOR: the minor version number
# OCAML_VERSION_PATCHLEVEL: the patchlevel number if present, or 0 if absent
# OCAML_VERSION_ADDITIONAL: this is defined only if the additional-info
#  field is present, and is a string that contains that field.
# Note that additional-info is always absent in officially-released
# versions of OCaml.

# usage:
# make-version-header.sh [version-file]
# The argument is the VERSION file from the OCaml sources.
# If the argument is not given, the version number from "ocamlc -v" will
# be used.

case $# in
  0) version="`ocamlc -v | sed -n -e 's/.*version //p'`";;
  1) version="`sed -e 1q $1`";;
  *) echo "usage: make-version-header.sh [version-file]" >&2
     exit 2;;
esac

major="`echo "$version" | sed -n -e '1s/^\([0-9]*\)\..*/\1/p'`"
minor="`echo "$version" | sed -n -e '1s/^[0-9]*\.\([0-9]*\).*/\1/p'`"
patchlvl="`echo "$version" | sed -n -e '1s/^[0-9]*\.[0-9]*\.\([0-9]*\).*/\1/p'`"
suffix="`echo "$version" | sed -n -e '1s/^[^+]*+\(.*\)/\1/p'`"

echo "#define OCAML_VERSION_MAJOR $major"
printf "#define OCAML_VERSION_MINOR %d\n" $minor
case $patchlvl in "") patchlvl=0;; esac
echo "#define OCAML_VERSION_PATCHLEVEL $patchlvl"
case "$suffix" in
  "") echo "#undef OCAML_VERSION_ADDITIONAL";;
  *) echo "#define OCAML_VERSION_ADDITIONAL \"$suffix\"";;
esac
printf "#define OCAML_VERSION %d%02d%02d\n" $major $minor $patchlvl
echo "#define OCAML_VERSION_STRING \"$version\""
