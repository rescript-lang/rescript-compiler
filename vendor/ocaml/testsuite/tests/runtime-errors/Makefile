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
	@$(MAKE) compile
	@$(MAKE) run

.PHONY: compile
compile:
	@for f in *.ml; do \
	  F=`basename $$f .ml`; \
	  rm -f $$F.bytecode $$F.native $$F.native.exe; \
	  $(OCAMLC) -w a -o $$F.bytecode $$f; \
	  if $(BYTECODE_ONLY); then : ; else \
	    $(OCAMLOPT) -w a -o $$F.native$(EXE) $$f; \
	  fi; \
	done
	$(if $(findstring win32,$(UNIX_OR_WIN32)),:, \
	@grep -q HAS_STACK_OVERFLOW_DETECTION $(TOPDIR)/byterun/caml/s.h \
	  || rm -f stackoverflow.native$(EXE))

# Cygwin doesn't allow the stack limit to be changed - the 4096 is
# intended to be larger than the its default stack size. The logic
# causes the test to be skipped if the stacksize cannot be brought
# below this value (uname -s value exits with an error status in Cygwin)
.PHONY: run
run:
	@ul=`ulimit -s`; \
	 if ( [ "$$ul" = "unlimited" ] || [ $$ul -gt 4096 ] ) ; then \
	   ulimit -s 1024 && ul=1 || ul=0 ; \
	 else \
	   ul=1; \
	 fi; \
	 for f in *.bytecode; do \
	  printf " ... testing '$$f':"; \
	  if [ $$ul -eq 1 ] ; then \
	    $(OCAMLRUN) ./$$f >$$f.result 2>&1 || true; \
	    DIFF="$(DIFF)" sh $$f.checker \
	    && echo " => passed" || echo " => failed"; \
	  else \
	    echo " => unexpected error"; \
	  fi; \
	  fn=`basename $$f bytecode`native; \
	  if $(BYTECODE_ONLY) || [ ! -f "$${fn}$(EXE)" ] ; then \
	    echo " ... testing '$$fn': => skipped" ; \
	  else \
	    printf " ... testing '$$fn':"; \
	    if [ $$ul -eq 1 ] ; then \
	      ./$${fn}$(EXE) >$$fn.result 2>&1 || true; \
	      DIFF="$(DIFF)" sh $$fn.checker \
	      && echo " => passed" || echo " => failed"; \
	    else \
	      echo " => unexpected error"; \
	    fi; \
	  fi; \
	done

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.bytecode *.native *.native.exe *.result

include $(BASEDIR)/makefiles/Makefile.common
