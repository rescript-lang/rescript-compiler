#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                  Jeremie Dimino, Jane Street Europe                   #
#                                                                       #
#   Copyright 2015 Jane Street Group LLC                                #
#                                                                       #
#   All rights reserved.  This file is distributed under the terms of   #
#   the GNU Lesser General Public License version 2.1, with the         #
#   special exception on linking described in the file ../LICENSE.      #
#                                                                       #
#########################################################################

BASEDIR=../..
COMPFLAGS=-I $(OTOPDIR)/parsing -I $(OTOPDIR)/utils \
          -I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LIBRARIES=../../../compilerlibs/ocamlcommon unix
MODULES=
MAIN_MODULE=test

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

# This test is a bit slow and there is little value in testing both
# versions so we run only the native code one:
NATIVECODE_ONLY=true
