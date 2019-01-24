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
	@if $(BYTECODE_ONLY); then $(MAKE) skip ; else \
	  $(MAKE) compile; \
	fi

.PHONY: skip
skip:
	@echo " ... testing 'missing_set_of_closures' => skipped"

.PHONY: compile
compile:
	@$(OCAMLOPT) -c a.ml
	@$(OCAMLOPT) -c b.ml
	@$(OCAMLOPT) -c b2.ml
	@cp b.cmx b.cmi b2.cmx b2.cmi dir/
	@cd dir; printf " ... testing 'missing_set_of_closures'"; \
	  $(OCAMLOPT) -w -58 -c c.ml \
            && echo " => passed" || echo " => failed"; \

.PHONY: promote
promote:

.PHONY: clean
clean: defaultclean
	@rm -f *.cmi *.cmx *.$(O) dir/*.cmi dir/*.cmx  dir/*.$(O)

include $(BASEDIR)/makefiles/Makefile.common
