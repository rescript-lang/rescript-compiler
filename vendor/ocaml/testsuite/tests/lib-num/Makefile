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
MODULES=test test_nats test_big_ints test_ratios test_nums test_io
MAIN_MODULE=end_test
LIBRARIES=nums
ADD_COMPFLAGS=-w a -I $(OTOPDIR)/otherlibs/num
LD_PATH=$(TOPDIR)/otherlibs/num

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
