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

.PHONY: compile
compile: compile-targets

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean

include ../makefiles/Makefile.common

.PHONY: compile-targets
compile-targets: testing.cmi testing.cmo
	@if $(BYTECODE_ONLY); then : ; else \
	  $(MAKE) testing.cmx; \
	fi
