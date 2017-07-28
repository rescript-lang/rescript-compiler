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

# Makefile for the portable graphics library

LIBNAME=graphics
COBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o \
  subwindow.o
CAMLOBJS=graphics.cmo graphicsX11.cmo
LINKOPTS=-cclib "\"$(X11_LINK)\""
LDOPTS=-ldopt "$(X11_LINK)"

EXTRACFLAGS=$(X11_INCLUDES)

include ../Makefile

depend:
	$(CC) -MM $(CFLAGS) *.c | sed -e 's, /[^ ]*\.h,,g' > .depend
	$(CAMLRUN) ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
