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
EXECNAME=program$(EXE)

ABCDFILES=backtrace.ml
OTHERFILES=backtrace2.ml raw_backtrace.ml \
           backtrace_deprecated.ml backtrace_slots.ml
OTHERFILESNOINLINING=backtraces_and_finalizers.ml

default:
	$(MAKE) byte
	@if $(BYTECODE_ONLY); then : ; else $(MAKE) native; fi

.PHONY: byte
byte:
	@for file in $(ABCDFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' with ocamlc and argument '$$arg':"; \
	    F="`basename $$file .ml`"; \
	    (OCAMLRUNPARAM=b=1 $(OCAMLRUN) $(EXECNAME) $$arg || true) \
	         >$$F.$$arg.byte.result 2>&1; \
	    $(DIFF) $$F.$$arg.reference $$F.$$arg.byte.result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done
	@for file in $(OTHERFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlc:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=b=1 $(OCAMLRUN) $(EXECNAME) $$arg || true) \
	       >$$F.byte.result 2>&1; \
	  $(DIFF) $$F.reference $$F.byte.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: native
native:
	@for file in $(ABCDFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' with ocamlopt and argument '$$arg':"; \
	    F="`basename $$file .ml`"; \
	    (OCAMLRUNPARAM=b=1 ./$(EXECNAME) $$arg || true) \
	         >$$F.$$arg.native.result 2>&1; \
	    $(DIFF) $$F.$$arg.reference $$F.$$arg.native.result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done
	@for file in $(OTHERFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=b=1 ./$(EXECNAME) $$arg || true) \
	       >$$F.native.result 2>&1; \
	  $(DIFF) $$F.reference $$F.native.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@for file in $(OTHERFILESNOINLINING); do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -inline 0 -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=b=1 ./$(EXECNAME) $$arg || true) \
	       >$$F.native.result 2>&1; \
	  $(DIFF) $$F.reference $$F.native.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program program.exe

include $(BASEDIR)/makefiles/Makefile.common
