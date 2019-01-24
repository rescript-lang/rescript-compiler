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

COMPFLAGS=-I $(OTOPDIR)/otherlibs/unix
LD_PATH=$(TOPDIR)/otherlibs/unix

.PHONY: default
default:
	@case " $(OTHERLIBRARIES) " in \
	  *' unix '*) $(SET_LD_PATH) $(MAKE) run-byte; \
	              $(SET_LD_PATH) $(MAKE) run-opt;; \
	  *) $(MAKE) skip;; \
	esac

.PHONY: common
common:
	@$(CC) -c $(CFLAGS) $(CPPFLAGS)  -I$(CTOPDIR)/byterun callbackprim.c

.PHONY: skip
skip:
	@for c in bytecode native; do \
	  echo " ... testing '$$c': => skipped" ; \
	done

.PHONY: run-byte
run-byte: common
	@printf " ... testing 'bytecode':"
	@$(OCAMLC) $(COMPFLAGS) -c tcallback.ml
	@$(OCAMLC) $(COMPFLAGS) -o ./program$(EXE) -custom unix.cma \
	           callbackprim.$(O) tcallback.cmo
	@./program$(EXE) >bytecode.result
	@$(DIFF) reference bytecode.result \
	&& echo " => passed" || echo " => failed"

.PHONY: run-opt
run-opt: common
	@if $(BYTECODE_ONLY); then : ; else \
	  printf " ... testing 'native':"; \
	  $(OCAMLOPT) $(COMPFLAGS) -c tcallback.ml; \
	  $(OCAMLOPT) $(COMPFLAGS) -o ./program$(EXE) unix.cmxa \
	              callbackprim.$(O) tcallback.cmx; \
	  ./program$(EXE) >native.result; \
	  $(DIFF) reference native.result \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result ./program$(EXE)

include $(BASEDIR)/makefiles/Makefile.common
