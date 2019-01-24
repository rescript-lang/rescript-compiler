#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..

.PHONY: default
default:
	@$(MAKE) compile
	@$(MAKE) run

.PHONY: compile
compile:
	@$(OCAMLC) -ccopt -I -ccopt $(CTOPDIR)/byterun cmstub.c
	@$(OCAMLC) -ccopt -I -ccopt $(CTOPDIR)/byterun cmmain.c
	@$(OCAMLC) -c cmcaml.ml
	@$(OCAMLC) -custom -o program cmstub.$(O) cmcaml.cmo cmmain.$(O)

.PHONY: run
run:
	@printf " ... testing 'cmmain':"
	@./program >program.result
	@$(DIFF) program.reference program.result >/dev/null \
	&& echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program

include $(BASEDIR)/makefiles/Makefile.common
