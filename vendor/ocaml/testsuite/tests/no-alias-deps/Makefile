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

default: b.cmi c.cmi d.cmi aliases.ml
	@$(OCAMLC) -c aliases.ml > aliases.ml.result 2>&1 || true
	@$(OBJINFO) aliases.cmo | \
	  sed -e "s/[a-f0-9]\{32\}/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/g" \
	  > aliases.cmo.result 2>&1 || true
	@for file in *.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.result

b.cmi: b.cmi.pre
	@cp b.cmi.pre b.cmi

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
COMPFLAGS = -no-alias-deps
