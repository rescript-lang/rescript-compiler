#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                     Thomas Refis, Jane Street Europe                   *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

default:
	@$(MAKE) byte
	@if $(BYTECODE_ONLY) ; then \
	  echo " ... testing native 'test.reference': => skipped"; \
	else \
	  $(MAKE) native; \
	fi

native:
	@printf " ... testing native 'test.reference':"
	@$(OCAMLOPT) -c submodule.ml
	@$(OCAMLOPT) -c aliases.ml
	@$(OCAMLOPT) -c external.mli external.ml
	@$(OCAMLOPT) -c external_for_pack.mli external_for_pack.ml
	@$(OCAMLOPT) -c test.ml
	@$(OCAMLOPT) -a submodule.cmx aliases.cmx external.cmx \
		external_for_pack.cmx -o mylib.cmxa
	@$(OCAMLOPT) -c -for-pack P use_in_pack.ml
	@$(OCAMLOPT) -pack use_in_pack.cmx -o p.cmx
	@$(OCAMLOPT) mylib.cmxa p.cmx test.cmx -o test.native
	@./test.native > test.result
	@$(DIFF) test.result test.reference >/dev/null \
	    && echo " => passed" || echo " => failed"

byte:
	@printf " ... testing byte 'test.reference':"
	@$(OCAMLC) -c submodule.ml
	@$(OCAMLC) -c aliases.ml
	@$(OCAMLC) -c external.mli external.ml
	@$(OCAMLC) -c external_for_pack.mli external_for_pack.ml
	@$(OCAMLC) -c test.ml
	@$(OCAMLC) -a submodule.cmo aliases.cmo external.cmo \
		external_for_pack.cmo -o mylib.cma
	@$(OCAMLC) -c -for-pack P use_in_pack.ml
	@$(OCAMLC) -pack use_in_pack.cmo -o p.cmo
	@$(OCAMLC) mylib.cma p.cmo test.cmo -o test.byte
	@$(OCAMLRUN) ./test.byte > test.result
	@$(DIFF) test.result test.reference >/dev/null \
	    && echo " => passed" || echo " => failed"

promote: defaultpromote

clean: defaultclean
	@rm -f *.result
	@rm -f test.native test.byte

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
COMPFLAGS = -no-alias-deps
