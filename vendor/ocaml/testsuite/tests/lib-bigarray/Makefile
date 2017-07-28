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
LIBRARIES=unix bigarray
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix \
              -I $(OTOPDIR)/otherlibs/bigarray
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/bigarray

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common
