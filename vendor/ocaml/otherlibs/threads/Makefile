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

# FIXME reduce redundancy by including ../Makefile

include ../../config/Makefile
CAMLRUN ?= ../../boot/ocamlrun
CAMLYACC ?= ../../boot/ocamlyacc

CFLAGS += $(SHAREDCCCOMPOPTS)
CPPFLAGS += -I../../byterun
ROOTDIR=../..
CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib \
      -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/unix
MKLIB=$(CAMLRUN) ../../tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A -bin-annot -g -safe-string

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=../../stdlib

LIB_OBJS=$(LIB)/camlinternalFormatBasics.cmo pervasives.cmo		\
  $(LIB)/array.cmo $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/bytes.cmo	\
  $(LIB)/string.cmo $(LIB)/sys.cmo $(LIB)/sort.cmo marshal.cmo		\
  $(LIB)/obj.cmo $(LIB)/int32.cmo $(LIB)/int64.cmo			\
  $(LIB)/nativeint.cmo $(LIB)/lexing.cmo $(LIB)/parsing.cmo		\
  $(LIB)/set.cmo $(LIB)/map.cmo $(LIB)/stack.cmo $(LIB)/queue.cmo	\
  $(LIB)/camlinternalLazy.cmo $(LIB)/lazy.cmo $(LIB)/stream.cmo		\
  $(LIB)/buffer.cmo $(LIB)/camlinternalFormat.cmo $(LIB)/printf.cmo	\
  $(LIB)/arg.cmo $(LIB)/printexc.cmo $(LIB)/gc.cmo $(LIB)/digest.cmo	\
  $(LIB)/random.cmo $(LIB)/hashtbl.cmo $(LIB)/format.cmo		\
  $(LIB)/scanf.cmo $(LIB)/callback.cmo $(LIB)/camlinternalOO.cmo	\
  $(LIB)/oo.cmo $(LIB)/camlinternalMod.cmo $(LIB)/genlex.cmo		\
  $(LIB)/weak.cmo $(LIB)/ephemeron.cmo $(LIB)/filename.cmo		\
  $(LIB)/complex.cmo $(LIB)/arrayLabels.cmo $(LIB)/listLabels.cmo	\
  $(LIB)/bytesLabels.cmo $(LIB)/stringLabels.cmo			\
  $(LIB)/moreLabels.cmo $(LIB)/stdLabels.cmo

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo $(UNIXLIB)/unixLabels.cmo

all: libvmthreads.a threads.cma stdlib.cma unix.cma

allopt:

libvmthreads.a: $(C_OBJS)
	$(MKLIB) -o threads -oc vmthreads $(C_OBJS)

threads.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads -oc vmthreads $(CAML_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

unix.cma: $(UNIXLIB_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o unix -linkall $(UNIXLIB_OBJS)

pervasives.cmo: pervasives.mli pervasives.cmi pervasives.ml
	$(CAMLC) ${COMPFLAGS} -nopervasives -c pervasives.ml

pervasives.mli: $(LIB)/pervasives.mli
	ln -s $(LIB)/pervasives.mli pervasives.mli

pervasives.cmi: $(LIB)/pervasives.cmi
	ln -s $(LIB)/pervasives.cmi pervasives.cmi

marshal.cmo: marshal.mli marshal.cmi marshal.ml
	$(CAMLC) ${COMPFLAGS} -c marshal.ml

marshal.mli: $(LIB)/marshal.mli
	ln -s $(LIB)/marshal.mli marshal.mli

marshal.cmi: $(LIB)/marshal.cmi
	ln -s $(LIB)/marshal.cmi marshal.cmi

unix.mli: $(UNIXLIB)/unix.mli
	ln -s -f $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s -f $(UNIXLIB)/unix.cmi unix.cmi

unix.cmo: unix.mli unix.cmi $(UNIXLIB)/unixLabels.cmo
	$(CAMLC) ${COMPFLAGS} -c unix.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libvmthreads.a dllvmthreads.so *.o
	rm -f pervasives.mli marshal.mli unix.mli

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)

CMIFILES=thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi

install:
	if test -f dllvmthreads.so; then \
	  cp dllvmthreads.so "$(INSTALL_STUBLIBDIR)"; \
	fi
	mkdir -p "$(INSTALL_LIBDIR)/vmthreads"
	cp libvmthreads.a "$(INSTALL_LIBDIR)/vmthreads"
	cd "$(INSTALL_LIBDIR)/vmthreads"; $(RANLIB) libvmthreads.a
	cp $(CMIFILES) $(CMIFILES:.cmi=.mli) $(CMIFILES:.cmi=.cmti) \
	   threads.cma stdlib.cma unix.cma "$(INSTALL_LIBDIR)/vmthreads"

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(CPPFLAGS) *.c > .depend
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
