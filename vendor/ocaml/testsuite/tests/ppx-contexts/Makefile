BASEDIR=../..

INCLUDES=\
  -I $(OTOPDIR)/parsing \
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/compilerlibs

myppx=$(shell $(CYGPATH) '$(OCAMLRUN)') ./program$(EXE)

.PHONY: run
run: program$(EXE) test.reference
	@echo " ... testing -thread and -vmthread are propagated to PPX:"
	@( $(OCAMLC) -c -thread -ppx '$(myppx)' test.ml \
	   && $(OCAMLC) -c -vmthread -ppx '$(myppx)' test.ml ) 2> test.result
	@$(DIFF) test.reference test.result >/dev/null \
	&& echo " => passed" || echo " => failed"

program$(EXE): program.ml Makefile
	@$(OCAMLC) -o program$(EXE) $(INCLUDES) ocamlcommon.cma ./program.ml

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f program$(EXE) test.result *.cm*

include $(BASEDIR)/makefiles/Makefile.common
