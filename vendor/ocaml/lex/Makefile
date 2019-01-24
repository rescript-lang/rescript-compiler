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

# The lexer generator
include ../config/Makefile
CAMLRUN ?= ../boot/ocamlrun
CAMLYACC ?= ../boot/ocamlyacc

ROOTDIR=..

ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
export OCAML_FLEXLINK:=
else
export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
endif

CAMLC=$(CAMLRUN) ../boot/ocamlc -strict-sequence -nostdlib -I ../boot \
      -use-prims ../byterun/primitives
CAMLOPT=$(CAMLRUN) ../ocamlopt -nostdlib -I ../stdlib
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error A \
          -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS=
YACCFLAGS=-v
CAMLLEX=$(CAMLRUN) ../boot/ocamllex
CAMLDEP=$(CAMLRUN) ../tools/ocamldep


OBJS=cset.cmo syntax.cmo parser.cmo lexer.cmo table.cmo lexgen.cmo \
     compact.cmo common.cmo output.cmo outputbis.cmo main.cmo

all: ocamllex
allopt: ocamllex.opt

ocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o ocamllex $(OBJS)

ocamllex.opt: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -o ocamllex.opt $(OBJS:.cmo=.cmx)

clean::
	rm -f ocamllex ocamllex.opt
	rm -f *.cmo *.cmi *.cmx *.cmt *.cmti *.$(O) *~

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

clean::
	rm -f parser.ml parser.mli parser.output

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) -slash *.mli *.ml > .depend

include .depend
