#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
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

default:
	@$(OCAMLC) -o program.byte alloc.ml
	@./program.byte
	@$(OCAMLOPT) -o program.native alloc.ml
	@./program.native

clean: defaultclean
	@rm -fr program.*

include $(BASEDIR)/makefiles/Makefile.common
