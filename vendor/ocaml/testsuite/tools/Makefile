#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Jeremie Dimino, Jane Street Europe                     *
#*                                                                        *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=..
MAIN=expect_test
PROG=$(MAIN)$(EXE)
COMPFLAGS=-I $(OTOPDIR)/parsing -I $(OTOPDIR)/utils \
          -I $(OTOPDIR)/driver -I $(OTOPDIR)/toplevel
LIBRARIES=../../compilerlibs/ocamlcommon \
          ../../compilerlibs/ocamlbytecomp \
          ../../compilerlibs/ocamltoplevel

$(PROG): $(MAIN).cmo
	$(OCAMLC) -linkall -o $(PROG) $(LIBRARIES:=.cma) $(MAIN).cmo

include $(BASEDIR)/makefiles/Makefile.common

.PHONY: clean
clean: defaultclean
	rm -f $(PROG)
