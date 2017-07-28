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
CUSTOM_MODULE=odoc_test
COMPFLAGS=-I $(OTOPDIR)/ocamldoc
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/str
DOCFLAGS=-I $(OTOPDIR)/stdlib $(COMPFLAGS)

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo 'skipped (shared libraries not available)'; \
	else \
	  $(SET_LD_PATH) $(MAKE) run; \
	fi

.PHONY: run
run: $(CUSTOM_MODULE).cmo
	@for file in t*.ml; do \
	  printf " ... testing '$$file'"; \
	  F="`basename $$file .ml`"; \
	  $(OCAMLDOC) $(DOCFLAGS) -hide-warnings -g $(CUSTOM_MODULE).cmo \
	              -o $$F.result $$file; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@$(OCAMLDOC) $(DOCFLAGS) -hide-warnings -html t*.ml 2>&1 \
	  | grep -v test_types_display || true
	@$(OCAMLDOC) $(DOCFLAGS) -hide-warnings -latex t*.ml 2>&1 \
	  | grep -v test_types_display || true

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.html *.tex *.log *.out *.sty *.toc *.css *.aux

include $(BASEDIR)/makefiles/Makefile.common
