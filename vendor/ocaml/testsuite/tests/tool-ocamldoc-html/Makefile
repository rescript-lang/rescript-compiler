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
run: *.mli *.ml
# Note that we strip both .ml and .mli extensions
	@for file in *.ml *.mli; do \
	  printf " ... testing '$$file'"; \
	  F="`basename $$file .mli`"; \
	  F="`basename $$F .ml`"; \
	  $(OCAMLDOC) $(DOCFLAGS) -colorize-code -hide-warnings -html $ \
	              -o index $$file; \
	  cp $$F.html $$F.result; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;\
# For linebreaks.mli, we also compare type_Linebreaks.html and not only
# the main html file
	@cp type_Linebreaks.html type_Linebreaks.result;\
	printf " ... testing 'type_Linebreak.html'";\
	$(DIFF) type_Linebreaks.reference type_Linebreaks.result\
	&& echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.html *.tex *.log *.out *.sty *.toc *.css *.aux

include $(BASEDIR)/makefiles/Makefile.common
