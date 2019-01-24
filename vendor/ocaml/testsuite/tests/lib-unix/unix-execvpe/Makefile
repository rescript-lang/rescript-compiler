BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
MAIN_MODULE=exec

test:
	@if grep -q HAS_EXECVPE $(OTOPDIR)/byterun/caml/s.h; \
        then echo " ... testing => skipped (using the system-provided execvpe())"; \
        else $(MAKE) compile && $(SET_LD_PATH) $(MAKE) myrun; \
        fi

myrun:
	@printf " ... testing with"
	@if $(NATIVECODE_ONLY); then : ; else \
	   printf " ocamlc"; \
	   ./exec.run "$(MYRUNTIME) ./program.byte$(EXE)" $(EXEC_ARGS) \
	                >$(MAIN_MODULE).result \
	   && $(DIFF) $(MAIN_MODULE).reference $(MAIN_MODULE).result \
	      >/dev/null; \
	 fi \
	&& if $(BYTECODE_ONLY); then : ; else \
	     printf " ocamlopt"; \
	     ./exec.run ./program.native$(EXE) $(EXEC_ARGS) \
	                             > $(MAIN_MODULE).result \
	     && $(DIFF) $(MAIN_MODULE).reference $(MAIN_MODULE).result \
	                >/dev/null; \
	   fi \
	&& echo " => passed" || echo " => failed"

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
