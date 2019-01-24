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

include ../config/Makefile
CAMLRUN ?= ../boot/ocamlrun
CAMLYACC ?= ../boot/ocamlyacc
TARGET_BINDIR ?= $(BINDIR)

COMPILER=../ocamlc
CAMLC=$(CAMLRUN) $(COMPILER)
COMPFLAGS=-strict-sequence -absname -w +a-4-9-41-42-44-45-48 \
          -g -warn-error A -bin-annot -nostdlib \
          -safe-string -strict-formats
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif
OPTCOMPILER=../ocamlopt
CAMLOPT=$(CAMLRUN) $(OPTCOMPILER)
CAMLDEP=$(CAMLRUN) ../tools/ocamldep

OBJS=camlinternalFormatBasics.cmo pervasives.cmo $(OTHERS)
OTHERS=list.cmo char.cmo uchar.cmo bytes.cmo string.cmo sys.cmo \
  sort.cmo marshal.cmo obj.cmo array.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  lexing.cmo parsing.cmo \
  set.cmo map.cmo stack.cmo queue.cmo \
  camlinternalLazy.cmo lazy.cmo stream.cmo \
  buffer.cmo camlinternalFormat.cmo printf.cmo \
  arg.cmo printexc.cmo gc.cmo \
  digest.cmo random.cmo hashtbl.cmo weak.cmo \
  format.cmo scanf.cmo callback.cmo \
  camlinternalOO.cmo oo.cmo camlinternalMod.cmo \
  genlex.cmo ephemeron.cmo \
  filename.cmo complex.cmo \
  arrayLabels.cmo listLabels.cmo bytesLabels.cmo \
  stringLabels.cmo moreLabels.cmo stdLabels.cmo \
  spacetime.cmo camlinternalBigarray.cmo

.PHONY: all
all: stdlib.cma std_exit.cmo camlheader target_camlheader camlheader_ur

ifeq "$(RUNTIMED)" "true"
all: camlheaderd
endif

ifeq "$(RUNTIMEI)" "true"
all: camlheaderi
endif

ifeq "$(PROFILING)" "true"
PROFILINGTARGET = prof
else
PROFILINGTARGET = noprof
endif

.PHONY: allopt
allopt: stdlib.cmxa std_exit.cmx allopt-$(PROFILINGTARGET)

.PHONY: allopt-noprof
allopt-noprof:

.PHONY: allopt-prof
allopt-prof: stdlib.p.cmxa std_exit.p.cmx
	rm -f std_exit.p.cmi

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)

.PHONY: install
install::
	cp stdlib.cma std_exit.cmo *.cmi *.cmt *.cmti *.mli *.ml \
	  camlheader_ur \
	  "$(INSTALL_LIBDIR)"
	cp target_camlheader "$(INSTALL_LIBDIR)/camlheader"

ifeq "$(RUNTIMED)" "true"
install::
	cp target_camlheaderd $(INSTALL_LIBDIR)
endif

ifeq "$(RUNTIMEI)" "true"
install::
	cp target_camlheaderi $(INSTALL_LIBDIR)
endif

.PHONY: installopt
installopt: installopt-default installopt-$(PROFILINGTARGET)

.PHONY: installopt-default
installopt-default:
	cp stdlib.cmxa stdlib.$(A) std_exit.$(O) *.cmx "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) stdlib.$(A)

.PHONY: installopt-noprof
installopt-noprof:

.PHONY: installopt-prof
installopt-prof:
	cp stdlib.p.cmxa stdlib.p.$(A) std_exit.p.cmx std_exit.p.$(O) \
	  "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) stdlib.p.$(A)

ifeq "$(UNIX_OR_WIN32)" "unix"
HEADERPROGRAM = header.c
else # Windows
HEADERPROGRAM = headernt.c
endif

CAMLHEADERS =\
  camlheader target_camlheader camlheader_ur \
  camlheaderd target_camlheaderd \
  camlheaderi target_camlheaderi

ifeq "$(HASHBANGSCRIPTS)" "true"
$(CAMLHEADERS): ../config/Makefile
	for suff in '' d i; do \
	  echo '#!$(BINDIR)/ocamlrun'$$suff > camlheader$$suff && \
	  echo '#!$(TARGET_BINDIR)/ocamlrun'$$suff >target_camlheader$$suff; \
	done && \
	echo '#!' | tr -d '\012' > camlheader_ur;
else # Hashbang scripts not supported

$(CAMLHEADERS): $(HEADERPROGRAM) ../config/Makefile

ifeq "$(UNIX_OR_WIN32)" "unix"
$(CAMLHEADERS):
	for suff in '' d i; do \
	  $(CC) $(CFLAGS) $(CPPFLAGS) -I../byterun $(LDFLAGS) \
	            -DRUNTIME_NAME='"$(BINDIR)/ocamlrun'$$suff'"' \
	            header.c $(OUTPUTEXE)tmpheader$(EXE) && \
	  strip tmpheader$(EXE) && \
	  mv tmpheader$(EXE) camlheader$$suff && \
	  $(CC) $(CFLAGS) $(CPPFLAGS) -I../byterun $(LDFLAGS) \
	            -DRUNTIME_NAME='"$(TARGET_BINDIR)/ocamlrun'$$suff'"' \
	            header.c $(OUTPUTEXE)tmpheader$(EXE) && \
	  strip tmpheader$(EXE) && \
	  mv tmpheader$(EXE) target_camlheader$$suff; \
	done && \
	cp camlheader camlheader_ur

else # Windows

# TODO: see whether there is a way to further merge the rules below
# with those above

camlheader target_camlheader camlheader_ur: headernt.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I../byterun \
	          -DRUNTIME_NAME='"ocamlrun"' $(OUTPUTOBJ)headernt.$(O) $<
	$(MKEXE) -o tmpheader.exe headernt.$(O) $(EXTRALIBS)
	rm -f camlheader.exe
	mv tmpheader.exe camlheader
	cp camlheader target_camlheader
	cp camlheader camlheader_ur

camlheaderd target_camlheaderd: headernt.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I../byterun \
	          -DRUNTIME_NAME='"ocamlrund"' $(OUTPUTOBJ)headernt.$(O) $<
	$(MKEXE) -o tmpheader.exe headernt.$(O) $(EXTRALIBS)
	mv tmpheader.exe camlheaderd
	cp camlheaderd target_camlheaderd

camlheaderi: headernt.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I../byterun \
	          -DRUNTIME_NAME='"ocamlruni"' $(OUTPUTOBJ)headernt.$(O)
	$(MKEXE) -o tmpheader.exe headernt.$(O) $(EXTRALIBS)
	mv tmpheader.exe camlheaderi

# TODO: do not call flexlink to build tmpheader.exe (we don't need
# the export table)

endif # ifeq "$(UNIX_OR_WIN32)" "unix"

endif # ifeq "$(HASHBANGSCRIPTS)" "true"

stdlib.cma: $(OBJS)
	$(CAMLC) -a -o $@ $^

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^

stdlib.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(CAMLOPT) -a -o $@ $^

sys.ml: sys.mlp ../VERSION
	sed -e "s|%%VERSION%%|`sed -e 1q ../VERSION | tr -d '\r'`|" sys.mlp > $@

.PHONY: clean
clean::
	rm -f sys.ml

clean::
	rm -f $(CAMLHEADERS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) `sh ./Compflags $@` -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) `sh ./Compflags $@` -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) `sh ./Compflags $@` -c $<

.ml.p.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) `sh ./Compflags $@` \
	           -p -c -o $*.p.cmx $<

# Dependencies on the compiler
COMPILER_DEPS=$(filter-out -use-prims, $(COMPILER))
$(OBJS) std_exit.cmo: $(COMPILER_DEPS)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER_DEPS)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: $(OPTCOMPILER)

# Dependencies on Pervasives (not tracked by ocamldep)
$(OTHERS) std_exit.cmo: pervasives.cmi
$(OTHERS:.cmo=.cmi) std_exit.cmi: pervasives.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: pervasives.cmi
$(OBJS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmi
$(OTHERS:.cmo=.cmx) std_exit.cmx: pervasives.cmx
$(OTHERS:.cmo=.p.cmx) std_exit.p.cmx: pervasives.cmx

clean::
	rm -f *.cm* *.$(O) *.$(A)
	rm -f *~
	rm -f camlheader*

include .depend

# Note that .p.cmx targets do not depend (for compilation) upon other
# .p.cmx files.  When the compiler imports another compilation unit,
# it looks for the .cmx file (not .p.cmx).
.PHONY: depend
depend:
	$(CAMLDEP) -slash *.mli *.ml > .depend
	$(CAMLDEP) -slash *.ml | sed -e 's/\.cmx : /.p.cmx : /g' >>.depend
