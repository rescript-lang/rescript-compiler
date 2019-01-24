#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                          Bernhard Schommer                             *
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


default:
	@$(MAKE) byte
	@if $(BYTECODE_ONLY); then $(MAKE) opt-skipped ; else \
          $(MAKE) opt; \
        fi

byte:
	@$(OCAMLC) unknown-file 2>&1 | grep "don't know what to do with unknown-file" \
	> unknown-file.byte.result  || true
	@for file in *.byte.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done

opt:
	@$(OCAMLOPT) unknown-file  2>&1 | grep "don't know what to do with unknown-file"\
	> unknown-file.opt.result || true
	@for file in *.opt.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done

opt-skipped:
	@for file in *.opt.reference; do \
	  printf " ... testing '$$file':"; \
          echo " => skipped"; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common
