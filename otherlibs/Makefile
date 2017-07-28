#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# Common Makefile for otherlibs on the Unix ports

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib \
        -I $(ROOTDIR)/stdlib
CFLAGS=-I$(ROOTDIR)/byterun $(SHAREDCCCOMPOPTS) $(EXTRACFLAGS)

include ../Makefile.shared
# Note .. is the current directory (this makefile is included from
# a subdirectory)
