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
COMPFLAGS=-I $(OTOPDIR)/ocamldoc
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/str
DOCFLAGS=-I $(OTOPDIR)/stdlib $(COMPFLAGS)\
	-latextitle "1,subsection*" \
	-latextitle "2,subsubsection*" \
	-latextitle "6,subsection*" \
	-latextitle "7,subsubsection*" \
	-latex-type-prefix "TYP" \
	-latex-module-prefix "" \
	-latex-module-type-prefix "" \
	-latex-value-prefix ""

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo 'skipped (shared libraries not available)'; \
	else \
	  $(SET_LD_PATH) $(MAKE) run; \
	fi

.PHONY: run
run: *.ml *.mli *.txt
	@for file in *.mli *.ml *.txt; do \
	  printf " ... testing '$$file'"; \
	  F="`basename $$file .mli`"; \
	  F="`basename $$F .ml`"; \
	  F="`basename $$F .txt`"; \
	  $(OCAMLDOC) $(DOCFLAGS) -hide-warnings -latex $ \
	              -o $$F.result $$file; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.html *.tex *.log *.out *.sty *.toc *.css *.aux

include $(BASEDIR)/makefiles/Makefile.common
