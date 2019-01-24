#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 2001 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

LIBNAME=graphics
COBJS=open.$(O) draw.$(O) events.$(O)
CAMLOBJS=graphics.cmo
WIN32LIBS=$(call SYSLIB,kernel32) $(call SYSLIB,gdi32) $(call SYSLIB,user32)
LINKOPTS=-cclib "\"$(WIN32LIBS)\""
LDOPTS=-ldopt "$(WIN32LIBS)"

include ../Makefile

graphics.ml: ../graph/graphics.ml
	cp ../graph/graphics.ml graphics.ml
graphics.mli: ../graph/graphics.mli
	cp ../graph/graphics.mli graphics.mli

.PHONY:
depend:

graphics.cmo: graphics.cmi
graphics.cmx: graphics.cmi
draw.$(O): libgraph.h
open.$(O): libgraph.h

clean:: partialclean
	rm -f graphics.ml graphics.mli
