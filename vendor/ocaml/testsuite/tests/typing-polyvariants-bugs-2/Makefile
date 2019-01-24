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
default:
	@printf " ... testing 'pr3918':"
	@($(OCAMLC) -c pr3918a.mli \
	&& $(OCAMLC) -c pr3918b.mli \
	&& $(OCAMLC) -c pr3918c.ml \
	&& echo " => passed") || echo " => failed"

clean: defaultclean

include $(BASEDIR)/makefiles/Makefile.common
