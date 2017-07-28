#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Damien Doligez, EPI Gallium, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 2013 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../../..
ADD_COMPFLAGS=-g -custom
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo 'skipped (shared libraries not available)'; \
	else \
	  $(MAKE) compile; \
	  $(SET_LD_PATH) OCAMLLIB=. $(MAKE) run; \
	fi

.PHONY: compile
compile: $(ML_FILES) $(CMO_FILES)
	@rm -f c$(EXE)
	@$(OCAMLC) $(ADD_COMPFLAGS) -c a.ml -for-pack foo
	@$(OCAMLC) $(ADD_COMPFLAGS) a.cmo -pack -o foo.cmo
	@$(OCAMLC) $(ADD_COMPFLAGS) -c b.ml
	@$(OCAMLC) $(ADD_COMPFLAGS) foo.cmo b.cmo -o c$(EXE)
	@mkdir -p compiler-libs
	@cp $(TOPDIR)/toplevel/topdirs.cmi compiler-libs/

.PHONY: run
run:
	@printf " ... testing with ocamlc"
	@rm -f noev.result
	@echo 'source input_script' | \
	 $(OCAMLRUN) `$(CYGPATH) $(TOPDIR)/debugger/ocamldebug$(EXE)` \
	             c$(EXE) >noev.raw.result 2>&1 \
	 && sed -e '/Debugger version/d' -e '/^Time:/d' \
	        -e '/Breakpoint [0-9]* at [0-9]*:/d' -e '$$d' \
	        noev.raw.result >noev.result \
	 && $(DIFF) noev.reference noev.result >/dev/null \
	 && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.cm* c$(EXE)
	@rm -rf compiler-libs

include $(BASEDIR)/makefiles/Makefile.common
