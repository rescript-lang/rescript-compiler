#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

LIBNAME=bigarray
EXTRACFLAGS=-I../$(UNIXLIB) -DIN_OCAML_BIGARRAY
EXTRACAMLFLAGS=-I ../$(UNIXLIB)
COBJS=bigarray_stubs.$(O) mmap_ba.$(O) mmap.$(O)
CAMLOBJS=bigarray.cmo

include ../Makefile

mmap.$(O): ../$(UNIXLIB)/mmap.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<
mmap_ba.$(O): ../unix/mmap_ba.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

.PHONY: depend

depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(CFLAGS) $(CPPFLAGS) *.c | sed -e 's/\.o/.$$(O)/g' > .depend
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
