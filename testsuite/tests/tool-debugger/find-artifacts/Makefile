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
MAIN_MODULE=debuggee
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
	@rm -rf out
	@rm -f program.byte program.byte.exe
	@mkdir out
	@$(OCAMLC) $(ADD_COMPFLAGS) $(ADD_CFLAGS) -o out/blah.cmo -c \
	           $(O_FILES) $(CMA_FILES) $(CMO_FILES) $(ADD_CMO_FILES) \
	           in/blah.ml
	@$(OCAMLC) $(ADD_COMPFLAGS) $(ADD_CFLAGS) -o out/foo.cmo -c \
	           $(O_FILES) $(CMA_FILES) $(CMO_FILES) $(ADD_CMO_FILES) \
	           -I out in/foo.ml
	@$(OCAMLC) $(ADD_COMPFLAGS) $(ADD_CFLAGS) -o program.byte$(EXE) \
	           $(O_FILES) $(CMA_FILES) $(CMO_FILES) $(ADD_CMO_FILES) \
						 out/blah.cmo out/foo.cmo
	@mkdir -p compiler-libs
	@cp $(TOPDIR)/toplevel/topdirs.cmi compiler-libs/

.PHONY: run
run:
	@printf " ... testing with ocamlc"
	@rm -f $(MAIN_MODULE).result
	@echo 'source input_script' | \
	 $(OCAMLRUN) `$(CYGPATH) $(TOPDIR)/debugger/ocamldebug$(EXE)` \
	             program.byte$(EXE) >$(MAIN_MODULE).raw.result 2>&1 \
	 && sed -e '/Debugger version/d' -e '/^Time:/d' \
	        -e '/Breakpoint [0-9]* at [0-9]*:/d' -e '$$d' \
	        $(MAIN_MODULE).raw.result >$(MAIN_MODULE).result \
	 && $(DIFF) $(MAIN_MODULE).reference $(MAIN_MODULE).result >/dev/null \
	 && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program.byte program.byte.exe \
	       program.native program.native.exe \
	       $(GENERATED_SOURCES) $(O_FILES) $(TEST_TEMP_FILES)
	@rm -rf compiler-libs out

include $(BASEDIR)/makefiles/Makefile.common
