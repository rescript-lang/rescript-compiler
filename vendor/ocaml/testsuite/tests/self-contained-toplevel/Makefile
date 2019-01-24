#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                  Jeremie Dimino, Jane Street Europe                    *
#*                                                                        *
#*   Copyright 2016 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
LIBRARIES=ocaml
MODULES=foo cached_cmi
MAIN_MODULE=main
COMPFLAGS=-I $(OTOPDIR)/typing -I $(OTOPDIR)/toplevel
LIBRARIES=../../../compilerlibs/ocamlcommon \
          ../../../compilerlibs/ocamlbytecomp \
          ../../../compilerlibs/ocamltoplevel

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

BYTECODE_ONLY=true
GENERATED_SOURCES+=cached_cmi.ml
EXEC_ARGS=$(OCFLAGS) -noinit input.ml

cached_cmi.ml: foo.cmi gen_cached_cmi.ml
	@$(OCAML) ../../../compilerlibs/ocamlcommon.cma -I $(OTOPDIR)/typing \
	          gen_cached_cmi.ml > $@
