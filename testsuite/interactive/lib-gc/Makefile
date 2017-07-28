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

default:
	@$(OCAMLC) -o program.byte alloc.ml
	@./program.byte
	@$(OCAMLOPT) -o program.native alloc.ml
	@./program.native

clean: defaultclean
	@rm -fr program.*

include $(BASEDIR)/makefiles/Makefile.common
