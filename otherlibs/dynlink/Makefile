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

# Makefile for the dynamic link library

# FIXME reduce redundancy by including ../Makefile

include ../../config/Makefile
CAMLRUN ?= ../../boot/ocamlrun
CAMLYACC ?= ../../boot/ocamlyacc

ROOTDIR   = ../..
OCAMLC    = $(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
OCAMLOPT  = $(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib -I $(ROOTDIR)/stdlib

INCLUDES=-I ../../utils -I ../../typing -I ../../bytecomp -I ../../asmcomp
COMPFLAGS=-w +33..39 -warn-error A -bin-annot -g -safe-string \
          -I ../../stdlib $(INCLUDES)

OBJS=dynlinkaux.cmo dynlink.cmo

COMPILEROBJS=\
  ../../utils/misc.cmo ../../utils/config.cmo ../../utils/clflags.cmo \
  ../../utils/tbl.cmo ../../utils/consistbl.cmo \
  ../../utils/terminfo.cmo ../../utils/warnings.cmo \
  ../../parsing/asttypes.cmi \
  ../../parsing/location.cmo ../../parsing/longident.cmo \
  ../../parsing/docstrings.cmo ../../parsing/ast_helper.cmo \
  ../../parsing/ast_mapper.cmo \
  ../../typing/ident.cmo ../../typing/path.cmo \
  ../../typing/primitive.cmo ../../typing/types.cmo \
  ../../typing/btype.cmo ../../typing/subst.cmo ../../typing/predef.cmo \
  ../../typing/datarepr.cmo ../../typing/cmi_format.cmo ../../typing/env.cmo \
  ../../bytecomp/lambda.cmo ../../bytecomp/instruct.cmo \
  ../../bytecomp/cmo_format.cmi ../../bytecomp/opcodes.cmo \
  ../../bytecomp/runtimedef.cmo ../../bytecomp/bytesections.cmo \
  ../../bytecomp/dll.cmo ../../bytecomp/meta.cmo \
  ../../bytecomp/symtable.cmo

NATOBJS=dynlink.cmx

all: dynlink.cma extract_crc

allopt: dynlink.cmxa

dynlink.cma: $(OBJS)
	$(OCAMLC) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o dynlink.cma \
	         $(OBJS)

dynlink.cmxa: $(NATOBJS)
	$(OCAMLOPT) $(COMPFLAGS) -ccopt "$(NATDYNLINKOPTS)" -a -o dynlink.cmxa \
	           $(NATOBJS)

dynlinkaux.cmo: $(COMPILEROBJS)
	$(OCAMLC) $(COMPFLAGS) -pack -o dynlinkaux.cmo $(COMPILEROBJS)

dynlinkaux.cmi: dynlinkaux.cmo

dynlink.cmx: dynlink.cmi natdynlink.ml
	cp natdynlink.ml  dynlink.mlopt
	$(OCAMLOPT) -c $(COMPFLAGS) -impl dynlink.mlopt
	rm -f dynlink.mlopt

extract_crc: dynlink.cma extract_crc.cmo
	$(OCAMLC) -o extract_crc dynlink.cma extract_crc.cmo

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)

install:
	cp dynlink.cmi dynlink.cma dynlink.mli $(INSTALL_LIBDIR)
	cp extract_crc $(INSTALL_LIBDIR)/extract_crc$(EXE)

installopt:
	if $(NATDYNLINK); then \
	  cp $(NATOBJS) dynlink.cmxa dynlink.$(A) $(INSTALL_LIBDIR) && \
	  cd $(INSTALL_LIBDIR) && $(RANLIB) dynlink.$(A); \
	fi

partialclean:
	rm -f extract_crc *.cm[ioaxt] *.cmti *.cmxa

clean: partialclean
	rm -f *.$(A) *.$(O) *.so *.dll dynlink.mlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(OCAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(COMPFLAGS) $<

depend:

dynlink.cmo: dynlinkaux.cmi dynlink.cmi
extract_crc.cmo: dynlink.cmi
