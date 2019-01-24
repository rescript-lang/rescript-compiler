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
GENERATED= a.ml b.ml c.ml d.mli e.ml f.ml g.ml test

default: pr7325 pr6372 pr7563

pr7325:
	@printf " ... testing 'pr7325':"
	@echo "type _ t = T" > a.ml
	@echo "type 'a t = 'a A.t" > b.ml
	@echo 'external f : unit -> unit B.t = "%identity"' > c.ml
	@$(OCAMLC) -c a.ml b.ml && rm a.cmi && $(OCAMLC) -c c.ml \
	  && echo " => passed" || echo " => failed"

pr6372:
	@printf " ... testing 'pr6372':"
	@echo "type _ t =  C: { f: ('a -> [<\`X]) t } -> [<\`X] t" > d.mli
	@echo "open D;; let f (C {f}) = ()" > e.ml
	@$(OCAMLC) -c d.mli e.ml \
	  && echo " => passed" || echo " => failed"

pr7563:
	@printf " ... testing 'pr7563':"
	@echo "module A = struct end" > f.ml
	@echo "module Alias = A" >> f.ml
	@echo "exception Alias" >> f.ml
	@echo "let alias = Alias" >> f.ml
	@echo "exit (if F.Alias = F.alias then 0 else 1)" > g.ml
	@$(OCAMLC) f.ml g.ml -o test && $(OCAMLRUN) ./test \
	  && echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f $(GENERATED)

include $(BASEDIR)/makefiles/Makefile.common
