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

include ../config/Makefile
UNIXDIR=../otherlibs/$(UNIXLIB)
CAMLRUN ?= ../boot/ocamlrun
CAMLYACC ?= ../boot/ocamlyacc

CAMLC=$(CAMLRUN) ../ocamlc -nostdlib -I ../stdlib
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48-40 -warn-error A \
          -safe-string -strict-sequence -strict-formats
LINKFLAGS=-linkall -I $(UNIXDIR)
YACCFLAGS=
CAMLLEX=$(CAMLRUN) ../boot/ocamllex
CAMLDEP=$(CAMLRUN) ../tools/ocamldep
DEPFLAGS=$(INCLUDES)

INSTALL_BINDIR=$(DESTDIR)$(BINDIR)

INCLUDES=\
  -I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../toplevel \
  -I ../driver -I $(UNIXDIR)

OTHEROBJS=\
  $(UNIXDIR)/unix.cma \
  ../utils/config.cmo ../utils/tbl.cmo ../utils/misc.cmo \
  ../utils/identifiable.cmo ../utils/numbers.cmo \
  ../utils/arg_helper.cmo ../utils/clflags.cmo \
  ../utils/consistbl.cmo ../utils/warnings.cmo \
  ../utils/terminfo.cmo \
  ../parsing/location.cmo ../parsing/longident.cmo ../parsing/docstrings.cmo \
  ../parsing/syntaxerr.cmo \
  ../parsing/ast_helper.cmo ../parsing/ast_mapper.cmo \
  ../parsing/ast_iterator.cmo ../parsing/attr_helper.cmo \
  ../parsing/builtin_attributes.cmo \
  ../typing/ident.cmo ../typing/path.cmo ../typing/types.cmo \
  ../typing/btype.cmo ../typing/primitive.cmo ../typing/typedtree.cmo \
  ../typing/subst.cmo ../typing/predef.cmo \
  ../typing/datarepr.cmo ../typing/cmi_format.cmo ../typing/env.cmo \
  ../typing/oprint.cmo \
  ../typing/ctype.cmo ../typing/printtyp.cmo ../typing/mtype.cmo \
  ../typing/envaux.cmo \
  ../bytecomp/runtimedef.cmo ../bytecomp/bytesections.cmo \
  ../bytecomp/dll.cmo ../bytecomp/meta.cmo ../bytecomp/lambda.cmo ../bytecomp/symtable.cmo \
  ../bytecomp/opcodes.cmo ../driver/compdynlink.cmo \
  ../toplevel/genprintval.cmo


OBJS=\
	int64ops.cmo \
	primitives.cmo \
	unix_tools.cmo \
	debugger_config.cmo \
	parameters.cmo \
	lexer.cmo \
	input_handling.cmo \
	question.cmo \
	debugcom.cmo \
	exec.cmo \
	source.cmo \
	pos.cmo \
	checkpoints.cmo \
	events.cmo \
	program_loading.cmo \
	symbols.cmo \
	breakpoints.cmo \
	trap_barrier.cmo \
	history.cmo \
	printval.cmo \
	show_source.cmo \
	time_travel.cmo \
	program_management.cmo \
	frames.cmo \
	eval.cmo \
	show_information.cmo \
	loadprinter.cmo \
	parser.cmo \
	command_line.cmo \
	main.cmo

all: ocamldebug$(EXE)

ocamldebug$(EXE): $(OBJS) $(OTHEROBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamldebug$(EXE) -linkall $(OTHEROBJS) $(OBJS)

install:
	cp ocamldebug$(EXE) "$(INSTALL_BINDIR)/ocamldebug$(EXE)"

clean::
	rm -f ocamldebug$(EXE)
	rm -f *.cmo *.cmi

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) -slash $(DEPFLAGS) *.mli *.ml \
	| sed -e 's,$(UNIXDIR)/,$$(UNIXDIR)/,' > .depend

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly
clean::
	rm -f parser.ml parser.mli
beforedepend:: parser.ml parser.mli

include .depend
