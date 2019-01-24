#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                  Jeremie Dimino, Jane Street Europe                    *
#*                                                                        *
#*   Copyright 2015 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
LIBRARIES=unix bigarray
MODULES=common
MAIN_MODULE=main
C_FILES=test_common stubs
C_INCLUDES=-I $(OTOPDIR)/otherlibs/bigarray
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/bigarray \
	      -I $(OTOPDIR)/otherlibs/$(UNIXLIB)

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

NATIVECODE_ONLY=true
NATIVECCCOMPOPTS+=-I $(OTOPDIR)/otherlibs/bigarray
GENERATED_SOURCES+=main.ml stubs.c

main.ml: gen_test.ml
	@$(OCAML) gen_test.ml ml > $@

stubs.c: gen_test.ml
	@$(OCAML) gen_test.ml c > $@

common.cmx: common.cmi

compile: stubs.c
