#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#   Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt  #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

include ../config/Makefile
CAMLRUN ?= ../boot/ocamlrun
CAMLYACC ?= ../boot/ocamlyacc

ROOTDIR   = ..
OCAMLC    = $(CAMLRUN) $(ROOTDIR)/ocamlc -nostdlib -I $(ROOTDIR)/stdlib
OCAMLOPT  = $(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib -I $(ROOTDIR)/stdlib
OCAMLDEP  = $(CAMLRUN) $(ROOTDIR)/tools/ocamldep
OCAMLLEX  = $(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CP        = cp
COMPFLAGS= -warn-error A -w L -w R -w Z -I ../otherlibs/$(UNIXLIB) -safe-string
LINKFLAGS= -I ../otherlibs/$(UNIXLIB)

PACK_CMO=\
  const.cmo \
  loc.cmo \
  discard_printf.cmo \
  signatures.cmi \
  my_std.cmo \
  my_unix.cmo \
  tags.cmo \
  display.cmo \
  log.cmo \
  shell.cmo \
  bool.cmo \
  glob_ast.cmo \
  glob_lexer.cmo \
  glob.cmo \
  lexers.cmo \
  param_tags.cmo \
  command.cmo \
  ocamlbuild_config.cmo \
  ocamlbuild_where.cmo \
  slurp.cmo \
  options.cmo \
  pathname.cmo \
  configuration.cmo \
  flags.cmo \
  hygiene.cmo \
  digest_cache.cmo \
  resource.cmo \
  rule.cmo \
  solver.cmo \
  report.cmo \
  tools.cmo \
  fda.cmo \
  findlib.cmo \
  ocaml_arch.cmo \
  ocaml_utils.cmo \
  ocaml_dependencies.cmo \
  ocaml_compiler.cmo \
  ocaml_tools.cmo \
  ocaml_specific.cmo \
  plugin.cmo \
  exit_codes.cmo \
  hooks.cmo \
  main.cmo

EXTRA_CMO=\
  ocamlbuild_plugin.cmo \
  ocamlbuild_executor.cmo \
  ocamlbuild_unix_plugin.cmo

PACK_CMX=$(PACK_CMO:.cmo=.cmx)
EXTRA_CMX=$(EXTRA_CMO:.cmo=.cmx)
EXTRA_CMI=$(EXTRA_CMO:.cmo=.cmi)

INSTALL_LIB=\
  ocamlbuildlib.cma \
  ocamlbuild.cmo \
  ocamlbuild_pack.cmi \
  $(EXTRA_CMO:.cmo=.cmi)

INSTALL_LIB_OPT=\
  ocamlbuildlib.cmxa ocamlbuildlib.$(A) \
  ocamlbuild.cmx ocamlbuild.$(O) \
  ocamlbuild_pack.cmx \
  $(EXTRA_CMO:.cmo=.cmx) $(EXTRA_CMO:.cmo=.$(O))

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)/ocamlbuild
INSTALL_BINDIR=$(DESTDIR)$(BINDIR)

all: ocamlbuild.byte ocamlbuildlib.cma
                 # ocamlbuildlight.byte ocamlbuildlightlib.cma
allopt: ocamlbuild.native ocamlbuildlib.cmxa

# The executables

ocamlbuild.byte: ocamlbuild_pack.cmo $(EXTRA_CMO) ocamlbuild.cmo
	$(OCAMLC) $(LINKFLAGS) -o ocamlbuild.byte \
          unix.cma ocamlbuild_pack.cmo $(EXTRA_CMO) ocamlbuild.cmo

ocamlbuildlight.byte: ocamlbuild_pack.cmo ocamlbuildlight.cmo
	$(OCAMLC) $(LINKFLAGS) -o ocamlbuildlight.byte \
          ocamlbuild_pack.cmo ocamlbuildlight.cmo

ocamlbuild.native: ocamlbuild_pack.cmx $(EXTRA_CMX) ocamlbuild.cmx
	$(OCAMLOPT) $(LINKFLAGS) -o ocamlbuild.native \
          unix.cmxa ocamlbuild_pack.cmx $(EXTRA_CMX) ocamlbuild.cmx

# The libraries

ocamlbuildlib.cma: ocamlbuild_pack.cmo $(EXTRA_CMO)
	$(OCAMLC) -a -o ocamlbuildlib.cma \
          ocamlbuild_pack.cmo $(EXTRA_CMO)

ocamlbuildlightlib.cma: ocamlbuild_pack.cmo ocamlbuildlight.cmo
	$(OCAMLC) -a -o ocamlbuildlightlib.cma \
          ocamlbuild_pack.cmo ocamlbuildlight.cmo

ocamlbuildlib.cmxa: ocamlbuild_pack.cmx $(EXTRA_CMX)
	$(OCAMLOPT) -a -o ocamlbuildlib.cmxa \
          ocamlbuild_pack.cmx $(EXTRA_CMX)

# The packs

ocamlbuild_pack.cmo: $(PACK_CMO)
	$(OCAMLC) -pack $(PACK_CMO) -o ocamlbuild_pack.cmo

ocamlbuild_pack.cmi: ocamlbuild_pack.cmo

ocamlbuild_pack.cmx: $(PACK_CMX)
	$(OCAMLOPT) -pack $(PACK_CMX) -o ocamlbuild_pack.cmx

# The config file

ocamlbuild_config.ml: ../config/Makefile
	(echo 'let bindir = "$(BINDIR)"'; \
	 echo 'let libdir = "$(LIBDIR)"'; \
	 echo 'let supports_shared_libraries = $(SUPPORTS_SHARED_LIBRARIES)';\
	 echo 'let a = "$(A)"'; \
	 echo 'let o = "$(O)"'; \
	 echo 'let so = "$(SO)"'; \
	 echo 'let ext_dll = "$(EXT_DLL)"'; \
	 echo 'let exe = "$(EXE)"'; \
	) > ocamlbuild_config.ml
clean::
	rm -f ocamlbuild_config.ml
beforedepend:: ocamlbuild_config.ml

# The lexers

lexers.ml: lexers.mll
	$(OCAMLLEX) lexers.mll
clean::
	rm -f lexers.ml
beforedepend:: lexers.ml

glob_lexer.ml: glob_lexer.mll
	$(OCAMLLEX) glob_lexer.mll
clean::
	rm -f glob_lexer.ml
beforedepend:: glob_lexer.ml

# Installation

install:
	$(CP) ocamlbuild.byte $(INSTALL_BINDIR)/ocamlbuild$(EXE)
	$(CP) ocamlbuild.byte $(INSTALL_BINDIR)/ocamlbuild.byte$(EXE)
	mkdir -p $(INSTALL_LIBDIR)
	$(CP) $(INSTALL_LIB) $(INSTALL_LIBDIR)/

installopt:
	if test -f ocamlbuild.native; then $(MAKE) installopt_really; fi

installopt_really:
	$(CP) ocamlbuild.native $(INSTALL_BINDIR)/ocamlbuild$(EXE)
	$(CP) ocamlbuild.native $(INSTALL_BINDIR)/ocamlbuild.native$(EXE)
	mkdir -p $(INSTALL_LIBDIR)
	$(CP) $(INSTALL_LIB_OPT) $(INSTALL_LIBDIR)/

# The generic rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) -for-pack Ocamlbuild_pack $(COMPFLAGS) -c $<

clean::
	rm -f *.cm? *.$(O) *.cmxa *.$(A)
	rm -f *.byte *.native

# The dependencies

depend: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

$(EXTRA_CMI): ocamlbuild_pack.cmi
$(EXTRA_CMO): ocamlbuild_pack.cmo ocamlbuild_pack.cmi
$(EXTRA_CMX): ocamlbuild_pack.cmx ocamlbuild_pack.cmi

include .depend

.PHONY: all allopt clean beforedepend
.PHONY: install installopt installopt_really depend
