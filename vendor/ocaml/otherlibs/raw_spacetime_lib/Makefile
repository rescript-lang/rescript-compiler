#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*           Xavier Leroy, projet Cristal, INRIA Rocquencourt             *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for Raw_spacetime_lib

ROOTDIR=../..
include $(ROOTDIR)/config/Makefile

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib \
        -I $(ROOTDIR)/stdlib

# The remainder of this file could probably be simplified by including
# ../Makefile.

LIBNAME=raw_spacetime_lib
CAMLOBJS=raw_spacetime_lib.cmo

COMPFLAGS=-w +33..39 -warn-error A -bin-annot -g -safe-string $(EXTRACAMLFLAGS)

CMIFILES=$(CAMLOBJS:.cmo=.cmi)
CAMLOBJS_NAT=$(CAMLOBJS:.cmo=.cmx)

all: $(LIBNAME).cma $(CMIFILES)

allopt: $(LIBNAME).cmxa $(LIBNAME).$(CMXS) $(CMIFILES)

$(LIBNAME).cma: $(CAMLOBJS)
	$(CAMLC) -a -o $(LIBNAME).cma -linkall $(CAMLOBJS)

$(LIBNAME).cmxa: $(CAMLOBJS_NAT)
	$(CAMLOPT) -a -o $(LIBNAME).cmxa -linkall $(CAMLOBJS_NAT)

$(LIBNAME).cmxs: $(LIBNAME).cmxa
	$(CAMLOPT) -shared -o $(LIBNAME).cmxs -I . $(LIBNAME).cmxa

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)

install::
	cp $(LIBNAME).cma $(CMIFILES) $(CMIFILES:.cmi=.mli) $(INSTALL_LIBDIR)

installopt:
	cp $(CAMLOBJS_NAT) $(LIBNAME).cmxa $(LIBNAME).$(A) $(INSTALL_LIBDIR)/
	if test -f $(LIBNAME).cmxs; then \
	  cp $(LIBNAME).cmxs $(INSTALL_LIBDIR)/; \
	fi

partialclean:
	rm -f *.cm*

clean:: partialclean
	rm -f *.a *.o

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep *.mli *.ml > .depend

include .depend
