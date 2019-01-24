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
	@for file in *.txt; do \
	 TERM=dumb $(OCAML) -args $$file < test.ml 2>&1 \
			| grep -v '^        OCaml version' > $$file.result; \
	done
	@for file in *.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done


promote: defaultpromote

clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common
