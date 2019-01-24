#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *
#*                                                                        *
#*   Copyright 2001 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR = ..

include $(ROOTDIR)/config/Makefile
OCAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
OCAMLYACC ?= $(ROOTDIR)/boot/ocamlyacc

STDLIBFLAGS = -nostdlib -I $(ROOTDIR)/stdlib
OCAMLC    = $(OCAMLRUN) $(ROOTDIR)/ocamlc $(STDLIBFLAGS)
ifeq "$(UNIX_OR_WIN32)" "unix"
OCAMLOPT  = $(OCAMLRUN) $(ROOTDIR)/ocamlopt $(STDLIBFLAGS)
else # Windows
  ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
    FLEXLINK_ENV=
  else
    FLEXLINK_ENV=OCAML_FLEXLINK="$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe"
  endif
  OCAMLOPT = $(FLEXLINK_ENV) $(OCAMLRUN) $(ROOTDIR)/ocamlopt $(STDLIBFLAGS)
endif
OCAMLDEP  = $(OCAMLRUN) $(ROOTDIR)/tools/ocamldep -slash
OCAMLLEX  = $(OCAMLRUN) $(ROOTDIR)/boot/ocamllex
# TODO: figure out whether the DEBUG lines the following preprocessor removes
# are actually useful.
# If they are not, then the preprocessor logic (including the
# remove_DEBUG script and the debug target) could be removed.
# If they are, it may be better to be able to enable them at run-time
# rather than compile-time, e.g. through a -debug command-line option. 
# In the following line, "sh" is useful under Windows. Without it,
# the ./remove_DEBUG command would be executed by cmd.exe which would not
# know how to handle it.
OCAMLPP=-pp 'sh ./remove_DEBUG'

# For installation
##############

MKDIR=mkdir -p
CP=cp
OCAMLDOC=ocamldoc

# TODO: clarify whether the following really needs to be that complicated
ifeq "$(UNIX_OR_WIN32)" "unix"
  ifeq "$(TARGET)" "$(HOST)"
    ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
      OCAMLDOC_RUN=$(OCAMLRUN) -I $(ROOTDIR)/otherlibs/$(UNIXLIB) -I $(ROOTDIR)/otherlibs/str ./$(OCAMLDOC)
    else
      OCAMLDOC_RUN=./$(OCAMLDOC)
    endif
  else
    OCAMLDOC_RUN=$(OCAMLRUN) ./$(OCAMLDOC)
  endif
else # Windows
  OCAMLDOC_RUN = CAML_LD_LIBRARY_PATH="../otherlibs/win32unix;../otherlibs/str" $(OCAMLRUN) ./$(OCAMLDOC)
endif

OCAMLDOC_OPT=$(OCAMLDOC).opt
OCAMLDOC_LIBCMA=odoc_info.cma
OCAMLDOC_LIBCMI=odoc_info.cmi
OCAMLDOC_LIBCMXA=odoc_info.cmxa
OCAMLDOC_LIBA=odoc_info.$(A)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)/ocamldoc

INSTALL_BINDIR=$(DESTDIR)$(BINDIR)

#MANO: man ocamldoc
INSTALL_MANODIR=$(DESTDIR)$(MANDIR)/man3

INSTALL_MLIS=odoc_info.mli
INSTALL_CMIS=$(INSTALL_MLIS:.mli=.cmi)
INSTALL_CMTS=$(INSTALL_MLIS:.mli=.cmt) $(INSTALL_MLIS:.mli=.cmti)

ODOC_TEST=odoc_test.cmo
GENERATORS_CMOS= \
        generators/odoc_todo.cmo \
        generators/odoc_literate.cmo
ifeq "$(NATDYNLINK)" "true"
GENERATORS_CMXS = $(GENERATORS_CMOS:.cmo=.cmxs)
else
GENERATORS_CMXS =
endif

# Compilation
#############

INCLUDES_DEP=\
  -I $(ROOTDIR)/parsing \
  -I $(ROOTDIR)/utils \
  -I $(ROOTDIR)/typing \
  -I $(ROOTDIR)/driver \
  -I $(ROOTDIR)/bytecomp \
  -I $(ROOTDIR)/toplevel

INCLUDES_NODEP=\
  -I $(ROOTDIR)/stdlib \
  -I $(ROOTDIR)/compilerlibs \
  -I $(ROOTDIR)/otherlibs/str \
  -I $(ROOTDIR)/otherlibs/dynlink \
  -I $(ROOTDIR)/otherlibs/$(UNIXLIB) \
  -I $(ROOTDIR)/otherlibs/$(GRAPHLIB)

INCLUDES=$(INCLUDES_DEP) $(INCLUDES_NODEP)

COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error A -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS=$(INCLUDES) -nostdlib

CMOFILES=\
  odoc_config.cmo \
  odoc_messages.cmo \
  odoc_global.cmo \
  odoc_types.cmo \
  odoc_misc.cmo \
  odoc_text_parser.cmo \
  odoc_text_lexer.cmo \
  odoc_text.cmo \
  odoc_name.cmo \
  odoc_parameter.cmo \
  odoc_value.cmo \
  odoc_type.cmo \
  odoc_extension.cmo \
  odoc_exception.cmo \
  odoc_class.cmo \
  odoc_module.cmo \
  odoc_print.cmo \
  odoc_str.cmo \
  odoc_comments_global.cmo \
  odoc_parser.cmo \
  odoc_lexer.cmo \
  odoc_see_lexer.cmo \
  odoc_env.cmo \
  odoc_merge.cmo \
  odoc_sig.cmo \
  odoc_ast.cmo \
  odoc_control.cmo \
  odoc_inherit.cmo \
  odoc_search.cmo \
  odoc_scan.cmo \
  odoc_cross.cmo \
  odoc_comments.cmo \
  odoc_dep.cmo \
  odoc_analyse.cmo \
  odoc_info.cmo

CMXFILES = $(CMOFILES:.cmo=.cmx)
CMIFILES = $(CMOFILES:.cmo=.cmi)

EXECMOFILES=\
  $(CMOFILES) \
  odoc_dag2html.cmo \
  odoc_to_text.cmo \
  odoc_ocamlhtml.cmo \
  odoc_html.cmo \
  odoc_man.cmo \
  odoc_latex_style.cmo \
  odoc_latex.cmo \
  odoc_texi.cmo \
  odoc_dot.cmo \
  odoc_gen.cmo \
  odoc_args.cmo \
  odoc.cmo

EXECMXFILES = $(EXECMOFILES:.cmo=.cmx)
EXECMIFILES = $(EXECMOFILES:.cmo=.cmi)

LIBCMOFILES = $(CMOFILES)
LIBCMXFILES = $(LIBCMOFILES:.cmo=.cmx)
LIBCMIFILES = $(LIBCMOFILES:.cmo=.cmi)

STDLIB_MLIS=\
  ../stdlib/*.mli \
  ../parsing/*.mli \
  ../otherlibs/$(UNIXLIB)/unix.mli \
  ../otherlibs/str/str.mli \
  ../otherlibs/bigarray/bigarray.mli

.PHONY: all
all: lib exe generators manpages

manpages: generators

.PHONY: exe
exe: $(OCAMLDOC)

.PHONY: lib
lib: $(OCAMLDOC_LIBCMA) $(OCAMLDOC_LIBCMI) $(ODOC_TEST)

.PHONY: generators
generators: $(GENERATORS_CMOS)

.PHONY: opt.opt
opt.opt: exeopt libopt generatorsopt

.PHONY: exeopt
exeopt: $(OCAMLDOC_OPT)

.PHONY: libopt
libopt: $(OCAMLDOC_LIBCMXA) $(OCAMLDOC_LIBCMI)

.PHONY: generatorsopt
generatorsopt: $(GENERATORS_CMXS)

# TODO: the following debug target could be replaced by a DEBUG variable
.PHONY: debug
debug:
	$(MAKE) OCAMLPP=""

OCAMLDOC_LIBRARIES = unix str dynlink ocamlcommon

OCAMLDOC_BCLIBRARIES = $(OCAMLDOC_LIBRARIES:%=%.cma)
OCAMLDOC_NCLIBRARIES = $(OCAMLDOC_LIBRARIES:%=%.cmxa)

$(OCAMLDOC): $(EXECMOFILES)
	$(OCAMLC) -o $@ -linkall $(LINKFLAGS) $(OCAMLDOC_BCLIBRARIES) $^

$(OCAMLDOC_OPT): $(EXECMXFILES)
	$(OCAMLOPT) -o $@ -linkall $(LINKFLAGS) $(OCAMLDOC_NCLIBRARIES) $^

$(OCAMLDOC_LIBCMA): $(LIBCMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $^

$(OCAMLDOC_LIBCMXA): $(LIBCMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $^

.PHONY: manpages
manpages: stdlib_man/Pervasives.3o

.PHONY: html_doc
html_doc: stdlib_html/Pervasives.html

.PHONY: dot
dot: ocamldoc.dot

ocamldoc.dot: $(EXECMOFILES)
	$(OCAMLDOC_RUN) -dot -dot-reduce -o $@ $(INCLUDES) odoc*.ml

# Parsers and lexers dependencies :
###################################
odoc_text_parser.ml: odoc_text_parser.mly
odoc_text_parser.mli: odoc_text_parser.mly

odoc_parser.ml:	odoc_parser.mly
odoc_parser.mli:odoc_parser.mly

odoc_text_lexer.ml: odoc_text_lexer.mll

odoc_lexer.ml:odoc_lexer.mll

odoc_ocamlhtml.ml: odoc_ocamlhtml.mll

odoc_see_lexer.ml: odoc_see_lexer.mll


# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx .cmxs

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmxs:
	$(OCAMLOPT) -shared -o $@ $(OCAMLPP) $(COMPFLAGS) $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

# Installation targets
######################

# TODO: it may be good to split the following rule in several ones, e.g.
# install-programs, install-doc, install-libs

.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_MANODIR)"
	$(CP) $(OCAMLDOC) "$(INSTALL_BINDIR)/$(OCAMLDOC)$(EXE)"
	$(CP) ocamldoc.hva *.cmi $(OCAMLDOC_LIBCMA) "$(INSTALL_LIBDIR)"
	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_CMTS) "$(INSTALL_LIBDIR)"
	if test -d stdlib_man; then $(CP) stdlib_man/* "$(INSTALL_MANODIR)"; else : ; fi

# Note: at the moment, $(INSTALL_MANODIR) is created even if the doc has
# not been built. This is not clean and should be changed.

.PHONY: installopt
installopt:
	if test -f $(OCAMLDOC_OPT); then $(MAKE) installopt_really ; fi

.PHONY: installopt_really
installopt_really:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(CP) $(OCAMLDOC_OPT) "$(INSTALL_BINDIR)/$(OCAMLDOC_OPT)$(EXE)"
	$(CP) $(INSTALL_MLIS) $(INSTALL_CMIS) $(INSTALL_CMTS) "$(INSTALL_LIBDIR)"
	$(CP) ocamldoc.hva *.cmx $(OCAMLDOC_LIBA) $(OCAMLDOC_LIBCMXA) \
	  "$(INSTALL_LIBDIR)"

# TODO: also split into several rules

# Testing :
###########

.PHONY: test
test:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -html -colorize-code -sort -d $@ $(INCLUDES) -dump $@/ocamldoc.odoc odoc*.ml odoc*.mli -v
	$(MKDIR) $@-custom
	$(OCAMLDOC_RUN) -colorize-code -sort -d $@-custom $(INCLUDES) \
	-g generators/odoc_literate.cmo -g generators/odoc_todo.cmo \
	-load $@/ocamldoc.odoc -v

.PHONY: test_stdlib
test_stdlib:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -html -colorize-code -sort -d $@ $(INCLUDES) -dump $@/stdlib.odoc -keep-code \
	../stdlib/pervasives.ml ../stdlib/*.mli \
	../otherlibs/$(UNIXLIB)/unix.mli \
	../otherlibs/str/str.mli

.PHONY: test_stdlib_code
test_stdlib_code:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -html -colorize-code -sort -d $@ $(INCLUDES) -dump $@/stdlib.odoc -keep-code \
	`ls ../stdlib/*.ml | grep -v Labels` \
	../otherlibs/$(UNIXLIB)/unix.ml \
	../otherlibs/str/str.ml

.PHONY: test_framed
test_framed:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -g odoc_fhtml.cmo -sort -colorize-code -d $@ $(INCLUDES) odoc*.ml odoc*.mli

.PHONY: test_latex
test_latex:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -latex -sort -o $@/test.tex -d $@ $(INCLUDES) odoc*.ml \
	        odoc*.mli test2.txt ../stdlib/*.mli ../otherlibs/unix/unix.mli

.PHONY: test_latex_simple
test_latex_simple:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -latex -sort -o $@/test.tex -d $@ $(INCLUDES) \
	-latextitle 6,subsection -latextitle 7,subsubection \
	../stdlib/hashtbl.mli \
	../stdlib/arg.mli \
	../otherlibs/$(UNIXLIB)/unix.mli \
	../stdlib/map.mli

.PHONY: test_man
test_man:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -man -sort -d $@ $(INCLUDES) odoc*.ml odoc*.mli

.PHONY: test_texi
test_texi:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -texi -sort -d $@ $(INCLUDES) odoc*.ml odoc*.mli

stdlib_man/Pervasives.3o: $(OCAMLDOC) $(STDLIB_MLIS)
	$(MKDIR) stdlib_man
	$(OCAMLDOC_RUN) -man -d stdlib_man $(INCLUDES) \
	-t "OCaml library" -man-mini $(STDLIB_MLIS)

stdlib_html/Pervasives.html: $(STDLIB_MLIS)
	$(MKDIR) stdlib_html
	$(OCAMLDOC_RUN) -d stdlib_html -html $(INCLUDES) \
	-t "OCaml library" $^

.PHONY: autotest_stdlib
autotest_stdlib:
	$(MKDIR) $@
	$(OCAMLDOC_RUN) -g autotest/odoc_test.cmo\
	$(INCLUDES) -keep-code \
	../stdlib/pervasives.ml ../stdlib/*.mli \
	../otherlibs/$(UNIXLIB)/unix.mli \
	../otherlibs/str/str.mli

# backup, clean and depend :
############################

.PHONY: clean
clean:
	rm -f *~ \#*\#
	rm -f $(OCAMLDOC) $(OCAMLDOC_OPT) *.cma *.cmxa *.cmo *.cmi *.cmx *.cmt *.cmti *.$(A) *.$(O)
	rm -f odoc_parser.output odoc_text_parser.output
	rm -f odoc_lexer.ml odoc_text_lexer.ml odoc_see_lexer.ml odoc_ocamlhtml.ml
	rm -f odoc_parser.ml odoc_parser.mli odoc_text_parser.ml odoc_text_parser.mli
	rm -rf stdlib_man
	rm -f generators/*.cm[taiox] generators/*.$(A) generators/*.$(O) generators/*.cmx[as]

.PHONY: depend
depend:
	$(OCAMLYACC) odoc_text_parser.mly
	$(OCAMLYACC) odoc_parser.mly
	$(OCAMLLEX) odoc_text_lexer.mll
	$(OCAMLLEX) odoc_lexer.mll
	$(OCAMLLEX) odoc_ocamlhtml.mll
	$(OCAMLLEX) odoc_see_lexer.mll
	$(OCAMLDEP) $(INCLUDES_DEP) *.mll *.mly *.ml *.mli > .depend
	$(OCAMLDEP) $(INCLUDES_DEP) -shared generators/*.ml >> .depend

include .depend
