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

COMPFLAGS=-I $(OTOPDIR)/otherlibs/dynlink
LD_PATH=.:$(TOPDIR)/otherlibs/dynlink

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo 'skipped (shared libraries not available)'; \
	else \
	  $(SET_LD_PATH) $(MAKE) compile run; \
	fi

.PHONY: compile
compile:
	@$(OCAMLC) -c registry.ml
	@for file in stub*.c; do \
	  $(OCAMLC) -ccopt -I -ccopt $(CTOPDIR)/byterun/caml -c $$file; \
	  $(OCAMLMKLIB) -o `echo $$file | sed -e 's/stub/plug/' -e 's/\.c//'` \
	                `basename $$file c`$(O); \
	done
	@for file in plug*.ml; do \
	  $(OCAMLC) -c $$file; \
	  $(OCAMLMKLIB) -o `basename $$file .ml` `basename $$file ml`cmo; \
	done
	@$(OCAMLC) -c main.ml
	@rm -f main static custom custom.exe
	@$(OCAMLC) -o main dynlink.cma registry.cmo main.cmo
	@$(OCAMLC) -o static -linkall registry.cmo plug1.cma plug2.cma \
	           -use-runtime $(OTOPDIR)/boot/ocamlrun$(EXE)
	@$(OCAMLC) -o custom$(EXE) -custom -linkall registry.cmo plug2.cma \
	           plug1.cma -I .

.PHONY: run
run:
	@printf " ... testing 'main'"
	@$(OCAMLRUN) ./main plug1.cma plug2.cma >main.result
	@$(DIFF) main.reference main.result >/dev/null \
	&& echo " => passed" || echo " => failed"

	@printf " ... testing 'static'"
	@$(OCAMLRUN) ./static >static.result
	@$(DIFF) static.reference static.result >/dev/null \
	&& echo " => passed" || echo " => failed"

	@printf " ... testing 'custom'"
	@./custom$(EXE) >custom.result
	@$(DIFF) custom.reference custom.result >/dev/null \
	&& echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f main static custom custom.exe *.result marshal.data

include $(BASEDIR)/makefiles/Makefile.common
