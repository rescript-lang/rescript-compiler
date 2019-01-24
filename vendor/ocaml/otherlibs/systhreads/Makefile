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

ROOTDIR=../..

include $(ROOTDIR)/config/Makefile

CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/boot/ocamlyacc

ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
export OCAML_FLEXLINK:=
else
export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
endif

LIBS = -nostdlib -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/$(UNIXLIB)

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc $(LIBS)
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt $(LIBS)
MKLIB=$(CAMLRUN) $(ROOTDIR)/tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A -g -bin-annot -safe-string
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif

LIBNAME=threads

ifeq "$(UNIX_OR_WIN32)" "unix"
HEADER = st_posix.h
else # Windows
HEADER = st_win32.h
endif

# Note: the header on which object files produced from st_stubs.c
# should actually depend is known for sure only at compile-time.
# That's why this dependency is handled in the Makefile directly
# and removed from the output of the C compiler during make depend

BYTECODE_C_OBJS=st_stubs_b.$(O)
NATIVECODE_C_OBJS=st_stubs_n.$(O)

THREADS_SOURCES = thread.ml mutex.ml condition.ml event.ml threadUnix.ml

THREADS_BCOBJS = $(THREADS_SOURCES:.ml=.cmo)
THREADS_NCOBJS = $(THREADS_SOURCES:.ml=.cmx)

MLIFILES=thread.mli mutex.mli condition.mli event.mli threadUnix.mli
CMIFILES=$(MLIFILES:.mli=.cmi)

all: lib$(LIBNAME).$(A) $(LIBNAME).cma $(CMIFILES)

allopt: lib$(LIBNAME)nat.$(A) $(LIBNAME).cmxa $(CMIFILES)

lib$(LIBNAME).$(A): $(BYTECODE_C_OBJS)
	$(MKLIB) -o $(LIBNAME) $(BYTECODE_C_OBJS) $(PTHREAD_LINK)

lib$(LIBNAME)nat.$(A): $(NATIVECODE_C_OBJS)
	$(MKLIB) -o $(LIBNAME)nat $^

$(LIBNAME).cma: $(THREADS_BCOBJS)
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MKLIB) -o $(LIBNAME) -ocamlc '$(CAMLC)' -cclib -lunix -linkall $(PTHREAD_CAML_LINK) $^
# TODO: Figure out why -cclib -lunix is used here.
# It may be because of the threadsUnix module which is deprecated.
# It may hence be good to figure out whether this module shouldn't be
# removed, and then -cclib -lunix arguments.
else # Windows
	$(MKLIB) -o $(LIBNAME) -ocamlc "$(CAMLC)" -linkall $(PTHREAD_CAML_LINK) $^
endif

# See remark above: force static linking of libthreadsnat.a
$(LIBNAME).cmxa: $(THREADS_NCOBJS)
	$(CAMLOPT) -linkall -a -cclib -lthreadsnat $(PTHREAD_CAML_LINK) -o $@ $^

# Note: I removed "-cclib -lunix" from the line above.
# Indeed, if we link threads.cmxa, then we must also link unix.cmxa,
# which itself will pass -lunix to the C linker.  It seems more
# modular to me this way. -- Alain

# The following lines produce two object files st_stubs_b.$(O) and
# st_stubs_n.$(O) from the same source file st_stubs.c (it is compiled
# twice, each time with different options).

st_stubs_b.$(O): st_stubs.c $(HEADER)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I$(ROOTDIR)/byterun  \
	  $(SHAREDCCCOMPOPTS) $(OUTPUTOBJ)$@ $<

st_stubs_n.$(O): st_stubs.c $(HEADER)
	$(CC) $(CFLAGS) $(CPPFLAGS) -I$(ROOTDIR)/asmrun -I$(ROOTDIR)/byterun \
	  $(SHAREDCCCOMPOPTS) -DNATIVE_CODE \
	  -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) \
	  $(OUTPUTOBJ)$@ -c $<

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f dllthreads*$(EXT_DLL) *.$(A) *.$(O)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
THREADS_LIBDIR=$(INSTALL_LIBDIR)/$(LIBNAME)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)

install:
	if test -f dllthreads$(EXT_DLL); then \
	  cp dllthreads$(EXT_DLL) "$(INSTALL_STUBLIBDIR)/dllthreads$(EXT_DLL)"; fi
	cp libthreads.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreads.$(A)
	mkdir -p "$(THREADS_LIBDIR)"
	cp $(CMIFILES) $(CMIFILES:.cmi=.cmti) threads.cma "$(THREADS_LIBDIR)"
	cp $(MLIFILES) "$(INSTALL_LIBDIR)"
	cp threads.h "$(INSTALL_LIBDIR)/caml"

installopt:
	cp libthreadsnat.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libthreadsnat.$(A)
	cp $(THREADS_NCOBJS) threads.cmxa threads.$(A) "$(THREADS_LIBDIR)"
	cd "$(THREADS_LIBDIR)" && $(RANLIB) threads.$(A)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend:
	$(CC) -MM $(CPPFLAGS) -I$(ROOTDIR)/byterun st_stubs.c \
	  | sed -e 's/st_stubs\.o/st_stubs_b.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' > .depend
	$(CC) -MM $(CPPFLAGS) -I$(ROOTDIR)/asmrun -I$(ROOTDIR)/byterun \
	  -DNATIVE_CODE -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) \
	  st_stubs.c | sed -e 's/st_stubs\.o/st_stubs_n.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' >> .depend
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
