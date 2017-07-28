#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..

COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix

MYRUNTIME=`if [ -z "$(CUSTOM)" ]; then echo '$(OCAMLRUN)'; fi`

.PHONY: default
default:
	@$(MAKE) compile
	@$(SET_LD_PATH) $(MAKE) run

.PHONY: compile
compile: tscanf2_io.cmo
	@rm -f master.byte master.native master.native.exe
	@rm -f slave.byte slave.native slave.native.exe
	@$(OCAMLC) unix.cma tscanf2_io.cmo -o master.byte tscanf2_master.ml
	@$(OCAMLC) tscanf2_io.cmo -o slave.byte tscanf2_slave.ml
	@if $(BYTECODE_ONLY); then : ; else \
	  $(MAKE) tscanf2_io.cmx; \
	  $(OCAMLOPT) unix.cmxa tscanf2_io.cmx -o master.native$(EXE) \
	              tscanf2_master.ml; \
	  $(OCAMLOPT) tscanf2_io.cmx -o slave.native$(EXE) tscanf2_slave.ml; \
	fi

run:
	@printf " ... testing with ocamlc"
	@$(MYRUNTIME) ./master.byte "$(OTOPDIR)/boot/ocamlrun$(EXE) \
	                                     `$(CYGPATH) ./slave.byte`" \
	             >result.byte 2>&1
	@$(DIFF) reference result.byte >/dev/null \
	&& if $(BYTECODE_ONLY); then : ; else \
	     printf " ocamlopt"; \
	     ./master.native$(EXE) "`$(CYGPATH) ./slave.native`" \
	                           >result.native 2>&1; \
	     $(DIFF) reference result.native >/dev/null; \
	   fi \
	&& echo " => passed" || echo " => failed"

.PHONY: promote
promote:
	@cp result.byte reference

.PHONY: clean
clean: defaultclean
	@rm -f master.* slave.* result.*

include $(BASEDIR)/makefiles/Makefile.common
