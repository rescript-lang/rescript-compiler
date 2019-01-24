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

# Common Makefile for otherlibs

ROOTDIR=../..
include $(ROOTDIR)/config/Makefile
CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/boot/ocamlyacc

ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
export OCAML_FLEXLINK:=
else
export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
endif

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib \
        -I $(ROOTDIR)/stdlib
CFLAGS += $(SHAREDCCCOMPOPTS) $(EXTRACFLAGS)
CPPFLAGS += -I$(ROOTDIR)/byterun

# Compilation options
COMPFLAGS=-absname -w +a-4-9-41-42-44-45-48 -warn-error A -bin-annot -g \
          -safe-string -strict-sequence -strict-formats $(EXTRACAMLFLAGS)
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif
MKLIB=$(CAMLRUN) $(ROOTDIR)/tools/ocamlmklib

# Variables to be defined by individual libraries:
#LIBNAME=
#CLIBNAME=
#CMIFILES=
#CAMLOBJS=
#COBJS=
#EXTRACFLAGS=
#EXTRACAMLFLAGS=
#LINKOPTS=
#LDOPTS=
#HEADERS=

CMIFILES ?= $(CAMLOBJS:.cmo=.cmi)
CAMLOBJS_NAT ?= $(CAMLOBJS:.cmo=.cmx)
CLIBNAME ?= $(LIBNAME)

all: lib$(CLIBNAME).$(A) $(LIBNAME).cma $(CMIFILES)

allopt: lib$(CLIBNAME).$(A) $(LIBNAME).cmxa $(LIBNAME).$(CMXS) $(CMIFILES)

$(LIBNAME).cma: $(CAMLOBJS)
	$(MKLIB) -o $(LIBNAME) -oc $(CLIBNAME) -ocamlc '$(CAMLC)' -linkall \
	         $(CAMLOBJS) $(LINKOPTS)

$(LIBNAME).cmxa: $(CAMLOBJS_NAT)
	$(MKLIB) -o $(LIBNAME) -oc $(CLIBNAME) -ocamlopt '$(CAMLOPT)' -linkall \
	         $(CAMLOBJS_NAT) $(LINKOPTS)

$(LIBNAME).cmxs: $(LIBNAME).cmxa lib$(CLIBNAME).$(A)
	$(CAMLOPT) -shared -o $(LIBNAME).cmxs -I . $(LIBNAME).cmxa

lib$(CLIBNAME).$(A): $(COBJS)
	$(MKLIB) -oc $(CLIBNAME) $(COBJS) $(LDOPTS)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)

install::
	if test -f dll$(CLIBNAME)$(EXT_DLL); then \
	  cp dll$(CLIBNAME)$(EXT_DLL) "$(INSTALL_STUBLIBDIR)/"; fi
	cp lib$(CLIBNAME).$(A) "$(INSTALL_LIBDIR)/"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) lib$(CLIBNAME).$(A)
	cp $(LIBNAME).cma $(CMIFILES) $(CMIFILES:.cmi=.mli) \
          $(CMIFILES:.cmi=.cmti) "$(INSTALL_LIBDIR)/"
	if test -n "$(HEADERS)"; then \
	  cp $(HEADERS) "$(INSTALL_LIBDIR)/caml/"; fi

installopt:
	cp $(CAMLOBJS_NAT) $(LIBNAME).cmxa $(LIBNAME).$(A) "$(INSTALL_LIBDIR)/"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) $(LIBNAME).a
	if test -f $(LIBNAME).cmxs; then \
	  cp $(LIBNAME).cmxs "$(INSTALL_LIBDIR)/"; fi

partialclean:
	rm -f *.cm*

clean:: partialclean
	rm -f *.dll *.so *.a *.lib *.o *.obj

.SUFFIXES: .ml .mli .cmi .cmo .cmx .$(O)

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

.c.$(O):
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<
