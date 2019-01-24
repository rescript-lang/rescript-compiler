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
EXECNAME=program$(EXE)

ABCDFILES=backtrace.ml
OTHERFILES=backtrace2.ml backtrace3.ml raw_backtrace.ml \
           backtrace_deprecated.ml backtrace_slots.ml
INLININGFILES=inline_test.ml inline_traversal_test.ml
OTHERFILESNOINLINING=pr6920_why_at.ml pr6920_why_swallow.ml
OTHERFILESNOINLINING_NATIVE=backtraces_and_finalizers.ml

# Keep only filenames, lines and character ranges
LOCATIONFILTER=grep -oE \
               '[a-zA-Z_]+\.ml(:[0-9]+)?|(line|characters) [0-9-]+'

default:
	@$(MAKE) byte
	@if $(BYTECODE_ONLY); then $(MAKE) skip ; else $(MAKE) native; fi

.PHONY: byte
byte:
	@for file in $(ABCDFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' with ocamlc and argument '$$arg':"; \
	    F="`basename $$file .ml`"; \
	    (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
              $(OCAMLRUN) $(EXECNAME) $$arg || true) \
	         >$$F.$$arg.byte.result 2>&1; \
	    $(DIFF) $$F.$$arg.byte.reference $$F.$$arg.byte.result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done
	@for file in $(OTHERFILES) $(OTHERFILESNOINLINING); do \
	  rm -f program program.exe; \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlc:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
            $(OCAMLRUN) $(EXECNAME) $$arg || true) \
	       >$$F.byte.result 2>&1; \
	  $(DIFF) $$F.byte.reference $$F.byte.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@for file in $(INLININGFILES); \
	do \
	  rm -f program program.exe; \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlc:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
	  $(OCAMLRUN) $(EXECNAME) $$arg 2>&1 || true) \
			| $(LOCATIONFILTER) >$$F.byte.result 2>&1; \
	  $(DIFF) $$F.byte.reference $$F.byte.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: skip
skip:
	@for file in $(ABCDFILES); do \
	  for arg in a b c d ''; do \
	    echo " ... testing '$$file' with ocamlopt and argument '$$arg': \
	=> skipped"; \
	  done; \
	done
	@for file in $(OTHERFILES) $(OTHERFILESNOINLINING) \
	             $(OTHERFILESNOINLINING_NATIVE) $(INLININGFILES); do \
	  echo " ... testing '$$file' with ocamlopt: => skipped"; \
	done

.PHONY: native
native:
	@for file in $(ABCDFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' with ocamlopt and argument '$$arg':";\
	    F="`basename $$file .ml`"; \
	    (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
              ./$(EXECNAME) $$arg || true) \
	         >$$F.$$arg.native.result 2>&1; \
	    $(DIFF) $$F.$$arg.native.reference $$F.$$arg.native.result \
	            >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done
	@for file in $(OTHERFILES); do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
            ./$(EXECNAME) $$arg || true) \
	       >$$F.native.result 2>&1; \
	  $(DIFF) $$F.native.reference $$F.native.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@for file in $(OTHERFILESNOINLINING) $(OTHERFILESNOINLINING_NATIVE); \
	do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -inline 0 -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
            ./$(EXECNAME) $$arg || true) \
	       >$$F.native.result 2>&1; \
	  $(DIFF) $$F.native.reference $$F.native.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@for file in $(INLININGFILES); \
	do \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) $$file; \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
	  ./$(EXECNAME) $$arg 2>&1 || true) \
		 	| $(LOCATIONFILTER) >$$F.native.result; \
	  $(DIFF) $$F.native.reference $$F.native.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	  rm -f program program.exe; \
	  $(OCAMLOPT) -g -o $(EXECNAME) -O3 $$file; \
	  printf " ... testing '$$file' with ocamlopt -O3:"; \
	  F="`basename $$file .ml`"; \
	  (OCAMLRUNPARAM=$$OCAMLRUNPARAM,b=1 \
	  ./$(EXECNAME) $$arg 2>&1 || true) \
		 	| $(LOCATIONFILTER) >$$F.O3.result; \
	  $(DIFF) $$F.native.reference $$F.O3.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done


.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program program.exe

include $(BASEDIR)/makefiles/Makefile.common
