#!/bin/sh
#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Damien Doligez, projet Para, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

exec %%BINDIR%%/ocamlc -I +compiler-libs -linkall ocamlcommon.cma \
                       ocamlbytecomp.cma ocamltoplevel.cma "$@" topstart.cmo
