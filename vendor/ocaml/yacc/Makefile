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

# Makefile for the parser generator.

include ../config/Makefile

OBJS= closure.$(O) error.$(O) lalr.$(O) lr0.$(O) main.$(O) \
  mkpar.$(O) output.$(O) reader.$(O) \
  skeleton.$(O) symtab.$(O) verbose.$(O) warshall.$(O)

all: ocamlyacc$(EXE)

ifeq ($(TOOLCHAIN),cc)
MKEXE_ANSI=$(MKEXE)
endif

ocamlyacc$(EXE): $(OBJS)
	$(MKEXE_ANSI) -o ocamlyacc$(EXE) $(OBJS) $(EXTRALIBS)

version.h : ../VERSION
	echo "#define OCAML_VERSION \"`sed -e 1q $^ | tr -d '\r'`\"" > $@

clean:
	rm -f *.$(O) ocamlyacc$(EXE) *~ version.h

depend:

closure.$(O): defs.h
error.$(O): defs.h
lalr.$(O): defs.h
lr0.$(O): defs.h
main.$(O): defs.h version.h
mkpar.$(O): defs.h
output.$(O): defs.h
reader.$(O): defs.h
skeleton.$(O): defs.h
symtab.$(O): defs.h
verbose.$(O): defs.h
warshall.$(O): defs.h

# The following rule is similar to make's default one, except that it
# also works for .obj files.

%.$(O): %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I../byterun $(OUTPUTOBJ)$@ $<
