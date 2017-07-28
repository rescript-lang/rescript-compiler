#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# Makefile for the parser generator.

include ../config/Makefile

CC=$(BYTECC)
CFLAGS=-DNDEBUG $(BYTECCCOMPOPTS)

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: ocamlyacc$(EXE)

ocamlyacc$(EXE): $(OBJS)
	$(CC) $(CFLAGS) $(CCLINKFLAGS) -o ocamlyacc$(EXE) $(OBJS)

version.h : ../VERSION
	echo "#define OCAML_VERSION \"`sed -e 1q ../VERSION`\"" >version.h

clean:
	rm -f *.o ocamlyacc$(EXE) *~ version.h

depend:

closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h version.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
