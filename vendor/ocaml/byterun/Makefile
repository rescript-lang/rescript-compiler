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

INSTALL_BINDIR=$(DESTDIR)$(BINDIR)
INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
INSTALL_INCDIR=$(INSTALL_LIBDIR)/caml

# The PROGRAMS (resp. LIBRARIES) variable list the files to build and
# install as programs in $(INSTALL_BINDIR) (resp. libraries in
# $(INSTALL_LIBDIR))

PROGRAMS = ocamlrun$(EXE)
LIBRARIES = ld.conf libcamlrun.$(A)

ifeq "$(RUNTIMED)" "true"
PROGRAMS += ocamlrund$(EXE)
LIBRARIES += libcamlrund.$(A)
endif

ifeq "$(RUNTIMEI)" "true"
PROGRAMS += ocamlruni$(EXE)
LIBRARIES += libcamlruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
LIBRARIES += libcamlrun_pic.$(A) libcamlrun_shared.$(SO)
endif
endif

ifdef BOOTSTRAPPING_FLEXLINK
CFLAGS += -DBOOTSTRAPPING_FLEXLINK
endif

# On Windows, OCAML_STDLIB_DIR needs to be defined dynamically

ifeq "$(UNIX_OR_WIN32)" "win32"
# OCAML_STDLIB_DIR needs to arrive in dynlink.c as a string which both gcc and
# msvc are willing parse without warning. This means we can't pass UTF-8
# directly since, as far as I can tell, cl can cope, but the pre-processor
# can't. So the string needs to be directly translated to L"" form. To do this,
# we take advantage of the fact that Cygwin uses GNU libiconv which includes a
# Java pseudo-encoding which translates any UTF-8 sequences to \uXXXX (and,
# unlike the C99 pseudo-encoding, emits two surrogate values when needed, rather
# than \UXXXXXXXX). The \u is then translated to \x in order to accommodate
# pre-Visual Studio 2013 compilers where \x is a non-standard alias for \u.
OCAML_STDLIB_DIR = $(shell echo $(LIBDIR)| iconv -t JAVA | sed -e 's/\\u/\\x/g')
CFLAGS += -DOCAML_STDLIB_DIR='L"$(OCAML_STDLIB_DIR)"'
endif

CFLAGS += $(IFLEXDIR)

ifneq "$(CCOMPTYPE)" "msvc"
CFLAGS += -g
endif

DFLAGS=$(CFLAGS) -DDEBUG
IFLAGS=$(CFLAGS) -DCAML_INSTR
PICFLAGS=$(CFLAGS) $(SHAREDCCCOMPOPTS)

DBGO=d.$(O)

ifeq "$(UNIX_OR_WIN32)" "win32"
LIBS = $(BYTECCLIBS) $(EXTRALIBS)
ifdef BOOTSTRAPPING_FLEXLINK
MAKE_OCAMLRUN=$(MKEXE_BOOT)
else
MAKE_OCAMLRUN = $(MKEXE) -o $(1) $(2)
endif
else
LIBS = $(BYTECCLIBS)
MAKE_OCAMLRUN = $(MKEXE) $(LDFLAGS) -o $(1) $(2)
endif

PRIMS=\
  alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c md5.c meta.c obj.c parsing.c \
  signals.c str.c sys.c terminfo.c callback.c weak.c finalise.c stacks.c \
  dynlink.c backtrace_prim.c backtrace.c spacetime.c afl.c

OBJS=$(addsuffix .$(O), \
  interp misc stacks fix_code startup_aux startup \
  freelist major_gc minor_gc memory alloc roots globroots \
  fail signals signals_byt printexc backtrace_prim backtrace \
  compare ints floats str array io extern intern \
  hash sys meta parsing gc_ctrl terminfo md5 obj \
  lexing callback debugger weak compact finalise custom \
  dynlink spacetime afl $(UNIX_OR_WIN32) bigarray main)

DOBJS=$(OBJS:.$(O)=.$(DBGO)) instrtrace.$(DBGO)
IOBJS=$(OBJS:.$(O)=.i.$(O))
PICOBJS=$(OBJS:.$(O)=.pic.$(O))

.PHONY: all
all: $(LIBRARIES) $(PROGRAMS)

ld.conf: ../config/Makefile
	echo "$(STUBLIBDIR)" > $@
	echo "$(LIBDIR)" >> $@

.PHONY: install
install:
	cp $(PROGRAMS) "$(INSTALL_BINDIR)"
	cp $(LIBRARIES) "$(INSTALL_LIBDIR)"
	mkdir -p "$(INSTALL_INCDIR)"
	cp caml/*.h "$(INSTALL_INCDIR)"

# If primitives contain duplicated lines (e.g. because the code is defined
# like
# #ifdef X
# CAMLprim value caml_foo() ...
# #else
# CAMLprim value caml_foo() ...
# end), horrible things will happen (duplicated entries in Runtimedef ->
# double registration in Symtable -> empty entry in the PRIM table ->
# the bytecode interpreter is confused).
# We sort the primitive file and remove duplicates to avoid this problem.

# Warning: we use "sort | uniq" instead of "sort -u" because in the MSVC
# port, the "sort" program in the path is Microsoft's and not cygwin's

# Warning: POSIX sort is locale dependent, that's why we set LC_ALL explicitly.
# Sort is unstable for "is_directory" and "isatty"
# see http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sort.html:
# "using sort to process pathnames, it is recommended that LC_ALL .. set to C"


primitives : $(PRIMS)
	sed -n -e "s/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p" $(PRIMS) \
	  | LC_ALL=C sort | uniq > primitives

prims.c : primitives
	(echo '#define CAML_INTERNALS'; \
         echo '#include "caml/mlvalues.h"'; \
	 echo '#include "caml/prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/	&,/' primitives; \
	 echo '	 0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/	"&",/' primitives; \
	 echo '	 0 };') > prims.c

caml/opnames.h : caml/instruct.h
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' caml/instruct.h \
	    > caml/opnames.h

# caml/jumptbl.h is required only if you have GCC 2.0 or later
caml/jumptbl.h : caml/instruct.h
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' caml/instruct.h > caml/jumptbl.h

caml/version.h : ../VERSION ../tools/make-version-header.sh
	../tools/make-version-header.sh ../VERSION > caml/version.h

.PHONY: clean
clean:
	rm -f $(LIBRARIES) $(PROGRAMS) *.$(O) *.$(A) *.$(SO)
	rm -f primitives prims.c caml/opnames.h caml/jumptbl.h
	rm -f caml/version.h

ocamlrun$(EXE): prims.$(O) libcamlrun.$(A)
	$(call MAKE_OCAMLRUN,$@,$^ $(LIBS))

libcamlrun.$(A): $(OBJS)
	$(call MKLIB,$@, $^)

ocamlrund$(EXE): prims.$(O) libcamlrund.$(A)
	$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(LIBS)

libcamlrund.$(A): $(DOBJS)
	$(call MKLIB,$@, $^)

ocamlruni$(EXE): prims.$(O) libcamlruni.$(A)
	$(MKEXE) -o $@ $^ $(LIBS)

libcamlruni.$(A): $(IOBJS)
	$(call MKLIB,$@, $^)

libcamlrun_pic.$(A): $(PICOBJS)
	$(call MKLIB,$@, $^)

libcamlrun_shared.$(SO): $(PICOBJS)
	$(MKDLL) -o $@ $^ $(BYTECCLIBS)

%.$(O): %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

%.$(DBGO): %.c
	$(CC) -c $(DFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.i.$(O): %.c
	$(CC) -c $(IFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.pic.$(O): %.c
	$(CC) -c $(PICFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend: prims.c caml/opnames.h caml/jumptbl.h caml/version.h
	$(CC) -MM $(CFLAGS) $(CPPFLAGS) *.c | sed -e 's/\.o/.$$(O)/'>.$@
	$(CC) -MM $(DFLAGS) $(CPPFLAGS) *.c | sed -e 's/\.o/.d.$$(O)/'>>.$@
	$(CC) -MM $(IFLAGS) $(CPPFLAGS) *.c | sed -e 's/\.o/.i.$$(O)/'>>.$@
	$(CC) -MM $(PICFLAGS) $(CPPFLAGS) *.c | sed -e 's/\.o/.pic.$$(O)/'>>.$@
endif

include .depend
