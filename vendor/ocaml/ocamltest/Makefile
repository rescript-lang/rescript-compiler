#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sebastien Hinderer, projet Gallium, INRIA Paris             *
#*                                                                        *
#*   Copyright 2016 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The Makefile for ocamltest

include ../config/Makefile

ifeq "$(UNIX_OR_WIN32)" "win32"
  ocamlsrcdir := $(shell echo "$(abspath $(shell pwd)/..)"|cygpath -m -f -)
else
  ocamlsrcdir := $(abspath $(shell pwd)/..)
endif

CPPFLAGS += -I../byterun -DCAML_INTERNALS

run := run_$(UNIX_OR_WIN32)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

sources := \
  $(run).c \
  run_stubs.c \
  ocamltest_config.mli ocamltest_config.ml.in \
  testlib.mli testlib.ml \
  run_command.mli run_command.ml \
  filetype.mli filetype.ml \
  filecompare.mli filecompare.ml \
  backends.mli backends.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  builtin_variables.mli builtin_variables.ml \
  builtin_modifiers.mli builtin_modifiers.ml \
  actions.mli actions.ml \
  builtin_actions.mli builtin_actions.ml \
  tests.mli tests.ml \
  builtin_tests.mli builtin_tests.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mli tsl_lexer.mll \
  tsl_semantics.mli tsl_semantics.ml \
  options.mli options.ml \
  main.mli main.ml

# List of .ml files used for ocamldep and to get the list of modules

ml_files := \
  $(filter %.ml, \
    $(subst .ml.in,.ml,$(subst .mll,.ml,$(subst .mly,.ml,$(sources)))) \
  )

cmo_files := $(ml_files:.ml=.cmo)

cmx_files := $(ml_files:.ml=.cmx)

ocaml_objects := $(ml_files:.ml=.$(O))

# List of .mli files for ocamldep
mli_files := \
  $(filter %.mli,$(subst .mly,.mli,$(sources)))

cmi_files := $(mli_files:.mli=.cmi)

c_files := $(filter %.c, $(sources))

o_files := $(c_files:.c=.$(O))

lexers := $(filter %.mll,$(sources))

parsers := $(filter %.mly,$(sources))

config_files := $(filter %.ml.in,$(sources))

dependencies_generated_prereqs := \
  $(config_files:.ml.in=.ml) \
  $(lexers:.mll=.ml) \
  $(parsers:.mly=.mli) $(parsers:.mly=.ml)

generated := $(dependencies_generated_prereqs) $(parsers:.mly=.output)

bytecode_modules := $(o_files) $(cmo_files)

native_modules := $(o_files) $(cmx_files)

directories = ../utils ../parsing ../stdlib ../compilerlibs

include_directories = $(addprefix -I , $(directories))

flags = -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

ifeq "$(UNIX_OR_WIN32)" "unix"
FLEXLINK_ENV=
else # Windows
  ifeq "$(wildcard ../flexdll/Makefile)" ""
    FLEXLINK_ENV=
  else
    FLEXLINK_ENV=OCAML_FLEXLINK="../boot/ocamlrun ../flexdll/flexlink.exe"
  endif
endif

ocamlc := $(FLEXLINK_ENV) ../byterun/ocamlrun ../ocamlc $(flags)

ocamlopt := $(FLEXLINK_ENV) ../byterun/ocamlrun ../ocamlopt $(flags)

ocamldep := ../byterun/ocamlrun ../tools/ocamldep -slash

ocamllex := ../byterun/ocamlrun ../lex/ocamllex

ocamlyacc := ../yacc/ocamlyacc

ocamlcdefaultflags :=

ocamloptdefaultflags := $(shell ./getocamloptdefaultflags $(TARGET))

ocamltest$(EXE): $(bytecode_modules)
	$(ocamlc) -custom ocamlcommon.cma -o $@ $^

%.cmo: %.ml
	$(ocamlc) -c $<

ocamltest.opt$(EXE): $(native_modules)
	$(ocamlopt) ocamlcommon.cmxa -o $@ $^

%.cmx: %.ml
	$(ocamlopt) -c $<

%.cmi: %.mli
	$(ocamlc) -c $<

%.ml %.mli: %.mly
	$(ocamlyacc) $<

%.ml: %.mll
	$(ocamllex) -q $<

%.$(O): %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(BYTECCCOMPOPTS) -c $<

ocamltest_config.ml: ocamltest_config.ml.in
	sed \
	  -e 's|@@ARCH@@|$(ARCH)|' \
	  -e 's|@@CPP@@|$(CPP)|' \
	  -e 's|@@OCAMLCDEFAULTFLAGS@@|$(ocamlcdefaultflags)|' \
	  -e 's|@@OCAMLOPTDEFAULTFLAGS@@|$(ocamloptdefaultflags)|' \
	  -e 's|@@OCAMLSRCDIR@@|$(ocamlsrcdir)|' \
	  -e 's|@@FLAMBDA@@|$(FLAMBDA)|' \
	  -e 's|@@FORCE_SAFE_STRING@@|$(FORCE_SAFE_STRING)|' \
	  $< > $@

.PHONY: clean
clean:
	rm -rf ocamltest$(EXE) ocamltest.opt$(EXE)
	rm -rf $(o_files) $(ocaml_objects)
	rm -rf $(cmi_files)
	rm -rf $(cmo_files)
	rm -rf $(cmx_files)
	rm -rf $(generated)

ifneq "$(TOOLCHAIN)" "msvc"
.PHONY: depend
depend: $(dependencies_generated_prereqs)
	$(CC) -MM $(CPPFLAGS) $(c_files) \
	  | sed -e 's/\.o/.$$(O)/' > .depend
	$(ocamldep) $(mli_files) $(ml_files) >> .depend
endif

-include .depend
