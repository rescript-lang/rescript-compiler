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

all: pr6939.ml
	$(MAKE) default

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.expect
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES = pr6939.ml *.flat-float

ifeq "$(FLAT_FLOAT_ARRAY)" "true"
suffix = -flat
else
suffix = -noflat
endif

pr6939.ml: pr6939.ml$(suffix) $(FLAT_FLOAT_ARRAY).flat-float
	cp $< $@

%.flat-float:
	@rm -f $(GENERATED_SOURCES)
	@touch $@
