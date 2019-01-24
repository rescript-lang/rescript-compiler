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

BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix

ifeq ($(OS),Windows_NT)
ADD_BYTERUN_FLAGS="-I $(OTOPDIR)/otherlibs/win32unix"
endif

default: reflector.exe fdstatus.exe cmdline_prog.exe
	@$(MAKE) check

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

%.exe: %.c
ifeq ($(CCOMPTYPE),msvc)
	@set -o pipefail ; \
	$(CC) $(CFLAGS) $(CPPFLAGS) /Fe$*.exe $*.c | tail -n +2
else
	@$(CC) $(CFLAGS) $(CPPFLAGS) -o $*.exe $*.c
endif
