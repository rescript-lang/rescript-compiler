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

# Makefile for the str library


LIBNAME=str
COBJS=strstubs.$(O)
CLIBNAME=camlstr
CAMLOBJS=str.cmo

include ../Makefile

depend:

str.cmo: str.cmi
str.cmx: str.cmi

depend:
	$(CC) -MM $(CFLAGS) *.c > .depend
	$(CAMLRUN) ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
