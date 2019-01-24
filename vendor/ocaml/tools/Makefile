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

MAKEFLAGS := -r -R
include ../config/Makefile
INSTALL_BINDIR:=$(DESTDIR)$(BINDIR)
INSTALL_LIBDIR:=$(DESTDIR)$(LIBDIR)
INSTALL_COMPLIBDIR:=$(DESTDIR)$(COMPLIBDIR)
INSTALL_STUBLIBDIR:=$(DESTDIR)$(STUBLIBDIR)
INSTALL_MANDIR:=$(DESTDIR)$(MANDIR)

ifeq ($(SYSTEM),unix)
override define shellquote
$i := $$(subst ",\",$$(subst $$$$,\$$$$,$$(subst `,\`,$i)))#")#
endef
$(foreach i,BINDIR LIBDIR COMPLIBDIR STUBLIBDIR MANDIR,$(eval $(shellquote)))
endif

CAMLRUN ?= ../boot/ocamlrun
CAMLYACC ?= ../boot/ocamlyacc
DESTDIR ?=
# Setup GNU make variables storing per-target source and target,
# a list of installed tools, and a function to quote a filename for
# the shell.
override installed_tools := ocamldep ocamlprof ocamlcp ocamloptp \
                   ocamlmktop ocamlmklib ocamlobjinfo

install_files :=
define byte2native
$(patsubst %.cmo,%.cmx,$(patsubst %.cma,%.cmxa,$1))
endef

# $1 = target, $2 = OCaml object dependencies, $3 = other dependencies
# There is a lot of subtle code here.  The multiple layers of expansion
# are due to `make`'s eval() function, which evaluates the string
# passed to it as a makefile fragment.  So it is crucial that variables
# not get expanded too many times.
define byte_and_opt_
# This check is defensive programming
$(and $(filter-out 1,$(words $1)),$(error \
   cannot build file with whitespace in name))
$1: $3 $2
	$$(CAMLC) $$(LINKFLAGS) -I .. -o $$@ $2

$1.opt: $3 $$(call byte2native,$2)
	$$(CAMLOPT) $$(LINKFLAGS) -I .. -o $$@ $$(call byte2native,$2)

all: $1

opt.opt: $1.opt

ifeq '$(filter $(installed_tools),$1)' '$1'
install_files += $1
endif
clean::
	rm -f -- $1 $1.opt

endef

# Escape any $ characters in the arguments and eval the result.
define byte_and_opt
$(eval $(call \
 byte_and_opt_,$(subst $$,$$$$,$1),$(subst $$,$$$$,$2),$(subst $$,$$$$,$3)))
endef

ROOTDIR=..

ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
export OCAML_FLEXLINK:=
else
export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
endif

CAMLC=$(CAMLRUN) ../boot/ocamlc -nostdlib -I ../boot \
      -use-prims ../byterun/primitives -I ..
CAMLOPT=$(CAMLRUN) ../ocamlopt -nostdlib -I ../stdlib
ifeq "$(UNIX_OR_WIN32)" "win32"
  ifneq "$(wildcard ../flexdll/Makefile)" ""
    CAMLOPT := OCAML_FLEXLINK="../boot/ocamlrun ../flexdll/flexlink.exe" \
      $(CAMLOPT)
  endif
endif
CAMLLEX=$(CAMLRUN) ../boot/ocamllex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp \
         -I ../middle_end -I ../middle_end/base_types -I ../driver \
         -I ../toplevel
COMPFLAGS= -absname -w +a-4-9-41-42-44-45-48 -strict-sequence -warn-error A \
 -safe-string -strict-formats -bin-annot $(INCLUDES)
LINKFLAGS=$(INCLUDES)
VPATH := $(filter-out -I,$(INCLUDES))

# scrapelabels addlabels

.PHONY: all opt.opt

# The dependency generator

CAMLDEP_OBJ=ocamldep.cmo
CAMLDEP_IMPORTS= \
  ../compilerlibs/ocamlcommon.cma \
  ../compilerlibs/ocamlbytecomp.cma
ocamldep: LINKFLAGS += -compat-32
$(call byte_and_opt,ocamldep,$(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ),)
ocamldep: depend.cmi
ocamldep.opt: depend.cmi

# ocamldep is precious: sometimes we are stuck in the middle of a
# bootstrap and we need to remake the dependencies
clean::
	if test -f ocamldep; then mv -f ocamldep ocamldep.bak; else :; fi
	rm -f ocamldep.opt


# The profiler

CSLPROF=ocamlprof.cmo
CSLPROF_IMPORTS=misc.cmo config.cmo identifiable.cmo numbers.cmo \
  arg_helper.cmo clflags.cmo terminfo.cmo \
  warnings.cmo location.cmo longident.cmo docstrings.cmo \
  syntaxerr.cmo ast_helper.cmo parser.cmo lexer.cmo parse.cmo

$(call byte_and_opt,ocamlprof,$(CSLPROF_IMPORTS) profiling.cmo $(CSLPROF),)

ocamlcp_cmos = misc.cmo profile.cmo warnings.cmo config.cmo identifiable.cmo \
               numbers.cmo arg_helper.cmo clflags.cmo main_args.cmo

$(call byte_and_opt,ocamlcp,$(ocamlcp_cmos) ocamlcp.cmo,)
$(call byte_and_opt,ocamloptp,$(ocamlcp_cmos) ocamloptp.cmo,)

opt:: profiling.cmx

install::
	cp -- profiling.cmi profiling.cmo profiling.cmt profiling.cmti "$(INSTALL_LIBDIR)"

installopt::
	cp -- profiling.cmx profiling.$(O) "$(INSTALL_LIBDIR)"

# To help building mixed-mode libraries (OCaml + C)

$(call byte_and_opt,ocamlmklib,ocamlmklibconfig.cmo config.cmo misc.cmo \
	         ocamlmklib.cmo,)


ocamlmklibconfig.ml: ../config/Makefile Makefile
	(echo 'let bindir = "$(BINDIR)"'; \
         echo 'let supports_shared_libraries = $(SUPPORTS_SHARED_LIBRARIES)';\
         echo 'let default_rpath = "$(RPATH)"'; \
         echo 'let mksharedlibrpath = "$(MKSHAREDLIBRPATH)"'; \
         echo 'let toolpref = "$(TOOLPREF)"'; \
         sed -n -e 's/^#ml //p' ../config/Makefile) \
        > ocamlmklibconfig.ml

beforedepend:: ocamlmklibconfig.ml

clean::
	rm -f ocamlmklibconfig.ml

# To make custom toplevels

OCAMLMKTOP=ocamlmktop.cmo
OCAMLMKTOP_IMPORTS=misc.cmo identifiable.cmo numbers.cmo config.cmo \
		   arg_helper.cmo clflags.cmo ccomp.cmo

$(call byte_and_opt,ocamlmktop,$(OCAMLMKTOP_IMPORTS) $(OCAMLMKTOP),)

# Converter olabl/ocaml 2.99 to ocaml 3

OCAML299TO3= lexer299.cmo ocaml299to3.cmo
LIBRARY3= misc.cmo warnings.cmo location.cmo

ocaml299to3: $(OCAML299TO3)
	$(CAMLC) $(LINKFLAGS) -o ocaml299to3 $(LIBRARY3) $(OCAML299TO3)

lexer299.ml: lexer299.mll
	$(CAMLLEX) lexer299.mll

#install::
#	cp ocaml299to3 "$(INSTALL_BINDIR)/ocaml299to3$(EXE)"

clean::
	rm -f ocaml299to3 lexer299.ml

# Label remover for interface files (upgrade 3.02 to 3.03)

SCRAPELABELS= lexer301.cmo scrapelabels.cmo

scrapelabels: $(SCRAPELABELS)
	$(CAMLC) $(LINKFLAGS) -o scrapelabels $(LIBRARY3) $(SCRAPELABELS)

lexer301.ml: lexer301.mll
	$(CAMLLEX) lexer301.mll

#install::
#	cp scrapelabels "$(INSTALL_LIBDIR)"

clean::
	rm -f scrapelabels lexer301.ml

# Insert labels following an interface file (upgrade 3.02 to 3.03)

ADDLABELS_IMPORTS=misc.cmo config.cmo arg_helper.cmo clflags.cmo \
  identifiable.cmo numbers.cmo terminfo.cmo \
  warnings.cmo location.cmo longident.cmo docstrings.cmo \
  syntaxerr.cmo ast_helper.cmo parser.cmo lexer.cmo parse.cmo

addlabels: addlabels.cmo
	$(CAMLC) $(LINKFLAGS) -w sl -o addlabels \
		$(ADDLABELS_IMPORTS) addlabels.cmo

#install::
#	cp addlabels "$(INSTALL_LIBDIR)"

ifeq ($(UNIX_OR_WIN32),unix)
LN := ln -sf
else
LN := cp -pf
endif

install::
	for i in $(install_files); \
	do \
	  cp -- "$$i" "$(INSTALL_BINDIR)/$$i.byte$(EXE)" && \
	  if test -f "$$i".opt; then \
	    cp -- "$$i.opt" "$(INSTALL_BINDIR)/$$i.opt$(EXE)" && \
	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  else \
	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.byte$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done

clean::
	rm -f addlabels

# The preprocessor for asm generators

CVT_EMIT=cvt_emit.cmo

cvt_emit: $(CVT_EMIT)
	$(CAMLC) $(LINKFLAGS) -o cvt_emit $(CVT_EMIT)

# cvt_emit is precious: sometimes we are stuck in the middle of a
# bootstrap and we need to remake the dependencies
.PRECIOUS: cvt_emit
clean::
	if test -f cvt_emit; then mv -f cvt_emit cvt_emit.bak; else :; fi

cvt_emit.ml: cvt_emit.mll
	$(CAMLLEX) cvt_emit.mll

clean::
	rm -f cvt_emit.ml

beforedepend:: cvt_emit.ml

# Reading cmt files

READ_CMT= \
          ../compilerlibs/ocamlcommon.cma \
          ../compilerlibs/ocamlbytecomp.cma \
          \
          cmt2annot.cmo read_cmt.cmo

# Reading cmt files
$(call byte_and_opt,read_cmt,$(READ_CMT),)

install::
	if test -f read_cmt.opt; then \
		cp read_cmt.opt "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	else \
		cp read_cmt "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	fi


# The bytecode disassembler

DUMPOBJ=opnames.cmo dumpobj.cmo

$(call byte_and_opt,dumpobj,misc.cmo identifiable.cmo numbers.cmo tbl.cmo \
                    config.cmo ident.cmo opcodes.cmo bytesections.cmo \
		    $(DUMPOBJ),)

make_opcodes.ml: make_opcodes.mll
	$(CAMLLEX) make_opcodes.mll

make_opcodes: make_opcodes.ml
	$(CAMLC) make_opcodes.ml -o $@

opnames.ml: ../byterun/caml/instruct.h make_opcodes
	$(CAMLRUN) make_opcodes -opnames < $< > $@

clean::
	rm -f opnames.ml make_opcodes make_opcodes.ml

beforedepend:: opnames.ml

# Display info on compiled files

ifeq "$(SYSTEM)" "macosx"
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
else
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix=""'
endif

objinfo_helper$(EXE): objinfo_helper.c ../byterun/caml/s.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -I../byterun $(OUTPUTEXE)$@ \
          $(DEF_SYMBOL_PREFIX) $(LIBBFD_INCLUDE) $< $(LIBBFD_LINK)

OBJINFO=../compilerlibs/ocamlcommon.cma \
        ../compilerlibs/ocamlbytecomp.cma \
        ../compilerlibs/ocamlmiddleend.cma \
        ../asmcomp/printclambda.cmo \
        ../asmcomp/export_info.cmo \
        objinfo.cmo

$(call byte_and_opt,ocamlobjinfo,$(OBJINFO),objinfo_helper$(EXE))

install::
	cp objinfo_helper$(EXE) "$(INSTALL_LIBDIR)/objinfo_helper$(EXE)"

# Scan object files for required primitives
$(call byte_and_opt,primreq,config.cmo primreq.cmo,)

LINTAPIDIFF=../compilerlibs/ocamlcommon.cmxa \
        ../compilerlibs/ocamlbytecomp.cmxa \
        ../compilerlibs/ocamlmiddleend.cmxa \
        ../asmcomp/printclambda.cmx \
        ../asmcomp/export_info.cmx \
	../otherlibs/str/str.cmxa \
	lintapidiff.cmx

lintapidiff.opt: INCLUDES+= -I ../otherlibs/str
lintapidiff.opt: $(LINTAPIDIFF)
	$(CAMLOPT) $(LINKFLAGS) -I .. -o $@ $(LINTAPIDIFF)
clean::
	rm -f -- lintapidiff.opt lintapidiff.cm? lintapidiff.o


clean::
	rm -f "objinfo_helper$(EXE)" "objinfo_helper$(EXE).manifest"


# Copy a bytecode executable, stripping debug info

stripdebug=../compilerlibs/ocamlcommon.cma \
           ../compilerlibs/ocamlbytecomp.cma \
           stripdebug.cmo

$(call byte_and_opt,stripdebug,$(stripdebug),)

# Compare two bytecode executables

CMPBYT=../compilerlibs/ocamlcommon.cma \
       ../compilerlibs/ocamlbytecomp.cma \
       cmpbyt.cmo

$(call byte_and_opt,cmpbyt,$(CMPBYT),)

ifeq "$(RUNTIMEI)" "true"
install::
	cp ocaml-instr-graph ocaml-instr-report "$(INSTALL_BINDIR)/"
endif

# Common stuff

.SUFFIXES:

%.cmo: %.ml
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmi: %.mli
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmx: %.ml
	$(CAMLOPT) $(COMPFLAGS) -c - $<

clean::
	rm -f *.cmo *.cmi *.cma *.dll *.so *.lib *.a

depend: beforedepend
	$(CAMLRUN) ./ocamldep -slash $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean install beforedepend depend

include .depend
