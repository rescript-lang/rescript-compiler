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
SHOULD_FAIL=t060-raise.ml

compile: lib.cmo
	@for file in t*.ml; do \
	  printf " ... testing '$$file'"; \
	  if [ `echo $(SHOULD_FAIL) | grep $$file` ]; then \
	    $(OCAML) -w a lib.cmo $$file 2>/dev/null \
	    && echo " => failed" || echo " => passed"; \
	  else \
	    $(OCAML) -w a lib.cmo $$file 2>/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  fi; \
	done

promote:

clean: defaultclean
	@rm -f ./a.out

include $(BASEDIR)/makefiles/Makefile.common
