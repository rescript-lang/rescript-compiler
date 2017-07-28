#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

include Makefile.common

CFLAGS=-DCAML_NAME_SPACE $(BYTECCCOMPOPTS) $(IFLEXDIR)
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS) $(IFLEXDIR)

OBJS=$(COMMONOBJS) $(UNIX_OR_WIN32).o main.o
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o
PICOBJS=$(OBJS:.o=.pic.o)

all:: all-$(SHARED)

ocamlrun$(EXE): libcamlrun.a prims.o
	$(MKEXE) $(BYTECCLINKOPTS) -o ocamlrun$(EXE) \
		  prims.o libcamlrun.a $(BYTECCLIBS)

ocamlrund$(EXE): libcamlrund.a prims.o
	$(MKEXE) $(MKEXEDEBUGFLAG) $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
		  prims.o libcamlrund.a $(BYTECCLIBS)

libcamlrun.a: $(OBJS)
	$(ARCMD) rc libcamlrun.a $(OBJS)
	$(RANLIB) libcamlrun.a

libcamlrund.a: $(DOBJS)
	$(ARCMD) rc libcamlrund.a $(DOBJS)
	$(RANLIB) libcamlrund.a

all-noshared:
.PHONY: all-noshared

all-shared: libcamlrun_pic.a libcamlrun_shared.so
.PHONY: all-shared

libcamlrun_pic.a: $(PICOBJS)
	ar rc libcamlrun_pic.a $(PICOBJS)
	$(RANLIB) libcamlrun_pic.a

libcamlrun_shared.so: $(PICOBJS)
	$(MKDLL) -o libcamlrun_shared.so $(PICOBJS) $(BYTECCLIBS)

install:: install-$(SHARED)

install-noshared:
.PHONY: install-noshared

install-shared:
	cp libcamlrun_shared.so $(INSTALL_LIBDIR)/libcamlrun_shared.so
	cp libcamlrun_pic.a $(INSTALL_LIBDIR)/libcamlrun_pic.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libcamlrun_pic.a
.PHONY: install-shared

clean::
	rm -f libcamlrun_shared.so libcamlrun_pic.a

%.d.o: %.c
	$(CC) -c $(DFLAGS) $< -o $@

%.pic.o: %.c
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $< -o $@

depend : prims.c caml/opnames.h caml/jumptbl.h caml/version.h
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c > .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend
.PHONY: depend

include .depend
