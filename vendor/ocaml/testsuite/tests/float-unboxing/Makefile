#(***********************************************************************)
#(*                                                                     *)
#(*                                OCaml                                *)
#(*                                                                     *)
#(*                   Mark Shinwell, Jane Street Europe                 *)
#(*                                                                     *)
#(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
#(*  en Automatique.  All rights reserved.  This file is distributed    *)
#(*  under the terms of the Q Public License version 1.0.               *)
#(*                                                                     *)
#(***********************************************************************)

BASEDIR=../..
MODULES=float_inline
MAIN_MODULE=float_subst_boxed_number
ADD_OPTCOMPFLAGS=-inline 20

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES=float_inline.ml *.flambda

float_inline.ml: $(FLAMBDA).flambda
ifeq ($(FLAMBDA),false)
	@echo "let eliminate_intermediate_float_record () = ()" > $@
else
	@cat float_flambda.ml > $@
endif

%.flambda:
	@rm -f float_inline.ml
	@touch $@
