#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Note: since this directory is Windows-specific, it may be good to make sure
# its content can not be compiled under Unix.
# This directory could even become a subdirectory of the unix directory.

# Files in this directory
WIN_FILES = accept.c bind.c channels.c close.c \
  close_on.c connect.c createprocess.c dup.c dup2.c errmsg.c envir.c \
  getpeername.c getpid.c getsockname.c gettimeofday.c isatty.c \
  link.c listen.c lockf.c lseek.c nonblock.c \
  mkdir.c mmap.c open.c pipe.c read.c readlink.c rename.c \
  select.c sendrecv.c \
  shutdown.c sleep.c socket.c sockopt.c startup.c stat.c \
  symlink.c system.c times.c unixsupport.c windir.c winwait.c write.c \
  winlist.c winworker.c windbug.c

# Files from the ../unix directory
UNIX_FILES = access.c addrofstr.c chdir.c chmod.c cst2constr.c \
  cstringv.c execv.c execve.c execvp.c \
  exit.c getaddrinfo.c getcwd.c gethost.c gethostname.c \
  getnameinfo.c getproto.c \
  getserv.c gmtime.c mmap_ba.c putenv.c rmdir.c \
  socketaddr.c strofaddr.c time.c unlink.c utimes.c

UNIX_CAML_FILES = unix.mli unixLabels.mli unixLabels.ml

ALL_FILES=$(WIN_FILES) $(UNIX_FILES)
WSOCKLIB=$(call SYSLIB,ws2_32)
ADVAPI32LIB=$(call SYSLIB,advapi32)

LIBNAME=unix
COBJS=$(ALL_FILES:.c=.$(O))
CAMLOBJS=unix.cmo unixLabels.cmo
LINKOPTS=-cclib $(WSOCKLIB) -cclib $(ADVAPI32LIB)
LDOPTS=-ldopt $(WSOCKLIB) -ldopt $(ADVAPI32LIB)
EXTRACAMLFLAGS=-nolabels
EXTRACFLAGS=-I../unix
HEADERS=unixsupport.h socketaddr.h


include ../Makefile

clean::
	rm -f $(UNIX_FILES) $(UNIX_CAML_FILES)

$(UNIX_FILES) $(UNIX_CAML_FILES): %: ../unix/%
	cp ../unix/$* $*

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend: $(ALL_FILES) $(UNIX_CAML_FILES) unix.ml
	$(CC) -MM $(CPPFLAGS) -I../unix $(ALL_FILES) \
	  | sed -e 's/\.o/.$$(O)/g' > .depend
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep -slash $(UNIX_CAML_FILES) \
	  unix.ml >> .depend
endif

include .depend
