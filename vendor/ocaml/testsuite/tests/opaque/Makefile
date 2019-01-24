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
	@echo " ... testing 'test' with ordinary compilation => skipped"
	@echo " ... testing 'test' with change to opaque interface => skipped"
	@echo " ... testing 'test' with change to opaque implementation \
	=> skipped"
	@echo " ... testing 'test' with change to non-opaque implementation \
	=> skipped"

.PHONY: compile
compile:
	@$(OCAMLOPT) -I intf -opaque -c intf/opaque_intf.mli
	@$(OCAMLOPT) -I intf -c intf/opaque_impl.mli
	@$(OCAMLOPT) -I intf -c intf/regular.mli
	@cp intf/*.mli intf/*.cmi fst
	@cp intf/*.mli intf/*.cmi snd
	@$(OCAMLOPT) -I fst -c fst/opaque_intf.ml
	@$(OCAMLOPT) -I fst -opaque -c fst/opaque_impl.ml
	@$(OCAMLOPT) -I fst -c fst/regular.ml
	@$(OCAMLOPT) -I snd -c snd/opaque_intf.ml
	@$(OCAMLOPT) -I snd -opaque -c snd/opaque_impl.ml
	@$(OCAMLOPT) -I snd -c snd/regular.ml
	@$(OCAMLOPT) -I fst -c test.ml
	@
	@printf " ... testing 'test' with ordinary compilation"; \
          $(OCAMLOPT) fst/opaque_intf.cmx fst/opaque_impl.cmx \
                        fst/regular.cmx test.cmx 2>/dev/null \
            && echo " => passed" || echo " => failed"; \
	 printf " ... testing 'test' with change to opaque interface"; \
          $(OCAMLOPT) snd/opaque_intf.cmx fst/opaque_impl.cmx \
                        fst/regular.cmx test.cmx 2>/dev/null \
            && echo " => passed" || echo " => failed"; \
	 printf " ... testing 'test' with change to opaque implementation"; \
	  $(OCAMLOPT) fst/opaque_intf.cmx snd/opaque_impl.cmx \
                        fst/regular.cmx test.cmx 2>/dev/null \
            && echo " => passed" || echo " => failed"; \
	 printf " ... testing 'test' with change to non-opaque implementation";\
	  $(OCAMLOPT) fst/opaque_intf.cmx fst/opaque_impl.cmx \
                        snd/regular.cmx test.cmx 2>/dev/null \
            && echo " => failed" || echo " => passed"; \

.PHONY: promote
promote:

.PHONY: clean
clean: defaultclean
	@rm -f *.cmi *.cmx *.$(O) a.out camlprog.exe
	@rm -f intf/*.cmi
	@rm -f fst/*.cmi fst/*.cmx fst/*.$(O) fst/*.mli
	@rm -f snd/*.cmi snd/*.cmx snd/*.$(O) snd/*.mli

include $(BASEDIR)/makefiles/Makefile.common
