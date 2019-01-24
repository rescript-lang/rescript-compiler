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

# The main Makefile

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make clean runtime coreall
# <debug your changes>
# make clean runtime coreall
# make coreboot [new system -- now in a stable state]

include config/Makefile

# For users who don't read the INSTALL file
.PHONY: defaultentry
defaultentry:
ifeq "$(UNIX_OR_WIN32)" "unix"
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world.opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."
else
	@echo "Please refer to the instructions in file README.win32.adoc."
endif

MKDIR=mkdir -p
ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

CAMLRUN ?= boot/ocamlrun
CAMLYACC ?= boot/ocamlyacc
include stdlib/StdlibModules

CAMLC=$(CAMLRUN) boot/ocamlc -g -nostdlib -I boot -use-prims byterun/primitives
CAMLOPT=$(CAMLRUN) ./ocamlopt -g -nostdlib -I stdlib -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I middle_end \
        -I middle_end/base_types -I asmcomp -I asmcomp/debug \
        -I driver -I toplevel

COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48-40 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats $(INCLUDES)
LINKFLAGS=

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

YACCFLAGS=-v --strict
CAMLLEX=$(CAMLRUN) boot/ocamllex
CAMLDEP=$(CAMLRUN) tools/ocamldep
DEPFLAGS=$(INCLUDES)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)

UTILS=utils/config.cmo utils/misc.cmo \
  utils/identifiable.cmo utils/numbers.cmo utils/arg_helper.cmo \
  utils/clflags.cmo utils/tbl.cmo utils/profile.cmo \
  utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo \
  utils/strongly_connected_components.cmo \
  utils/targetint.cmo

PARSING=parsing/location.cmo parsing/longident.cmo \
  parsing/docstrings.cmo parsing/syntaxerr.cmo \
  parsing/ast_helper.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo \
  parsing/pprintast.cmo \
  parsing/ast_mapper.cmo parsing/ast_iterator.cmo parsing/attr_helper.cmo \
  parsing/builtin_attributes.cmo parsing/ast_invariants.cmo parsing/depend.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/cmi_format.cmo typing/env.cmo \
  typing/typedtree.cmo typing/printtyped.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/envaux.cmo typing/includecore.cmo \
  typing/typedtreeIter.cmo typing/typedtreeMap.cmo \
  typing/tast_mapper.cmo \
  typing/cmt_format.cmo typing/untypeast.cmo \
  typing/includemod.cmo typing/typetexp.cmo typing/parmatch.cmo \
  typing/stypes.cmo typing/typedecl.cmo typing/typeopt.cmo typing/typecore.cmo \
  typing/typeclass.cmo \
  typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/semantics_of_primitives.cmo \
  bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translattribute.cmo \
  bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo \
  bytecomp/meta.cmo bytecomp/opcodes.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo \
  bytecomp/symtable.cmo \
  driver/pparse.cmo driver/main_args.cmo \
  driver/compenv.cmo driver/compmisc.cmo \
  driver/compdynlink.cmo driver/compplugin.cmo driver/makedepend.cmo


COMMON=$(UTILS) $(PARSING) $(TYPING) $(COMP)

BYTECOMP=bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/emitcode.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo bytecomp/bytepackager.cmo \
  driver/errors.cmo driver/compile.cmo

ARCH_SPECIFIC =\
  asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml asmcomp/selection.ml \
  asmcomp/scheduling.ml asmcomp/reload.ml

INTEL_ASM=\
  asmcomp/x86_proc.cmo \
  asmcomp/x86_dsl.cmo \
  asmcomp/x86_gas.cmo \
  asmcomp/x86_masm.cmo

ARCH_SPECIFIC_ASMCOMP=
ifeq ($(ARCH),i386)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif
ifeq ($(ARCH),amd64)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif

ASMCOMP=\
  $(ARCH_SPECIFIC_ASMCOMP) \
  asmcomp/arch.cmo \
  asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/debug/reg_with_debug_info.cmo \
  asmcomp/debug/reg_availability_set.cmo \
  asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/printclambda.cmo \
  asmcomp/export_info.cmo \
  asmcomp/export_info_for_pack.cmo \
  asmcomp/compilenv.cmo \
  asmcomp/closure.cmo \
  asmcomp/build_export_info.cmo \
  asmcomp/closure_offsets.cmo \
  asmcomp/flambda_to_clambda.cmo \
  asmcomp/import_approx.cmo \
  asmcomp/un_anf.cmo \
  asmcomp/afl_instrument.cmo \
  asmcomp/strmatch.cmo asmcomp/cmmgen.cmo \
  asmcomp/interval.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo \
  asmcomp/spacetime_profiling.cmo asmcomp/selection.cmo \
  asmcomp/comballoc.cmo \
  asmcomp/CSEgen.cmo asmcomp/CSE.cmo \
  asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/linscan.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/deadcode.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/debug/available_regs.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/branch_relaxation_intf.cmo \
  asmcomp/branch_relaxation.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo asmcomp/asmpackager.cmo \
  driver/opterrors.cmo driver/optcompile.cmo

MIDDLE_END=\
  middle_end/debuginfo.cmo \
  middle_end/base_types/tag.cmo \
  middle_end/base_types/linkage_name.cmo \
  middle_end/base_types/compilation_unit.cmo \
  middle_end/base_types/variable.cmo \
  middle_end/base_types/mutable_variable.cmo \
  middle_end/base_types/id_types.cmo \
  middle_end/base_types/set_of_closures_id.cmo \
  middle_end/base_types/set_of_closures_origin.cmo \
  middle_end/base_types/closure_element.cmo \
  middle_end/base_types/closure_id.cmo \
  middle_end/base_types/var_within_closure.cmo \
  middle_end/base_types/static_exception.cmo \
  middle_end/base_types/export_id.cmo \
  middle_end/base_types/symbol.cmo \
  middle_end/pass_wrapper.cmo \
  middle_end/allocated_const.cmo \
  middle_end/parameter.cmo \
  middle_end/projection.cmo \
  middle_end/flambda.cmo \
  middle_end/flambda_iterators.cmo \
  middle_end/flambda_utils.cmo \
  middle_end/inlining_cost.cmo \
  middle_end/effect_analysis.cmo \
  middle_end/freshening.cmo \
  middle_end/simple_value_approx.cmo \
  middle_end/lift_code.cmo \
  middle_end/closure_conversion_aux.cmo \
  middle_end/closure_conversion.cmo \
  middle_end/initialize_symbol_to_let_symbol.cmo \
  middle_end/lift_let_to_initialize_symbol.cmo \
  middle_end/find_recursive_functions.cmo \
  middle_end/invariant_params.cmo \
  middle_end/inconstant_idents.cmo \
  middle_end/alias_analysis.cmo \
  middle_end/lift_constants.cmo \
  middle_end/share_constants.cmo \
  middle_end/simplify_common.cmo \
  middle_end/remove_unused_arguments.cmo \
  middle_end/remove_unused_closure_vars.cmo \
  middle_end/remove_unused_program_constructs.cmo \
  middle_end/simplify_boxed_integer_ops.cmo \
  middle_end/simplify_primitives.cmo \
  middle_end/inlining_stats_types.cmo \
  middle_end/inlining_stats.cmo \
  middle_end/inline_and_simplify_aux.cmo \
  middle_end/remove_free_vars_equal_to_args.cmo \
  middle_end/extract_projections.cmo \
  middle_end/augment_specialised_args.cmo \
  middle_end/unbox_free_vars_of_closures.cmo \
  middle_end/unbox_specialised_args.cmo \
  middle_end/unbox_closures.cmo \
  middle_end/inlining_transforms.cmo \
  middle_end/inlining_decision.cmo \
  middle_end/inline_and_simplify.cmo \
  middle_end/ref_to_variables.cmo \
  middle_end/flambda_invariants.cmo \
  middle_end/middle_end.cmo

TOPLEVEL=toplevel/genprintval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

OPTTOPLEVEL=toplevel/genprintval.cmo toplevel/opttoploop.cmo \
  toplevel/opttopdirs.cmo toplevel/opttopmain.cmo
BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

OPTTOPLEVELSTART=toplevel/opttopstart.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev

COMPLIBDIR=$(LIBDIR)/compiler-libs

INSTALL_BINDIR=$(DESTDIR)$(BINDIR)
INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
INSTALL_COMPLIBDIR=$(DESTDIR)$(COMPLIBDIR)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)
INSTALL_MANDIR=$(DESTDIR)$(MANDIR)
INSTALL_FLEXDLL=$(INSTALL_LIBDIR)/flexdll

RUNTOP=./byterun/ocamlrun ./ocaml \
  -nostdlib -I stdlib \
  -noinit $(TOPFLAGS) \
  -I otherlibs/$(UNIXLIB)
NATRUNTOP=./ocamlnat$(EXE) -nostdlib -I stdlib -noinit $(TOPFLAGS)
ifeq "UNIX_OR_WIN32" "unix"
EXTRAPATH=
else
EXTRAPATH = PATH="otherlibs/win32unix:$(PATH)"
endif

BOOT_FLEXLINK_CMD=

ifeq "$(UNIX_OR_WIN32)" "win32"
FLEXDLL_SUBMODULE_PRESENT := $(wildcard flexdll/Makefile)
ifeq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
  BOOT_FLEXLINK_CMD=
  FLEXDLL_DIR=
else
  BOOT_FLEXLINK_CMD = FLEXLINK_CMD="../boot/ocamlrun ../flexdll/flexlink.exe"
  CAMLOPT := OCAML_FLEXLINK="boot/ocamlrun flexdll/flexlink.exe" $(CAMLOPT)
  FLEXDLL_DIR=$(if $(wildcard flexdll/flexdll_*.$(O)),+flexdll)
endif
else
  FLEXDLL_DIR=
endif

# The configuration file

# SUBST generates the sed substitution for the variable *named* in $1
# SUBST_QUOTE does the same, adding double-quotes around non-empty strings
#   (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty but an OCaml
#    string otherwise)
SUBST_ESCAPE=$(subst ",\\",$(subst \,\\,$(if $2,$2,$($1))))
SUBST=-e 's|%%$1%%|$(call SUBST_ESCAPE,$1,$2)|'
SUBST_QUOTE2=-e 's|%%$1%%|$(if $2,"$2")|'
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$(call SUBST_ESCAPE,$1,$2))
FLEXLINK_LDFLAGS=$(if $(LDFLAGS), -link "$(LDFLAGS)")
utils/config.ml: utils/config.mlp config/Makefile Makefile
	sed $(call SUBST,AFL_INSTRUMENT) \
	    $(call SUBST,ARCH) \
	    $(call SUBST,ARCMD) \
	    $(call SUBST,ASM) \
	    $(call SUBST,ASM_CFI_SUPPORTED) \
	    $(call SUBST,BYTECCLIBS) \
	    $(call SUBST,BYTERUN) \
	    $(call SUBST,CC) \
	    $(call SUBST,CCOMPTYPE) \
	    $(call SUBST,CC_PROFILE) \
	    $(call SUBST,OUTPUTOBJ) \
	    $(call SUBST,EXT_ASM) \
	    $(call SUBST,EXT_DLL) \
	    $(call SUBST,EXE) \
	    $(call SUBST,EXT_LIB) \
	    $(call SUBST,EXT_OBJ) \
	    $(call SUBST,FLAMBDA) \
	    $(call SUBST,FLEXLINK_FLAGS) \
	    $(call SUBST_QUOTE,FLEXDLL_DIR) \
	    $(call SUBST,HOST) \
	    $(call SUBST,LIBDIR) \
	    $(call SUBST,LIBUNWIND_AVAILABLE) \
	    $(call SUBST,LIBUNWIND_LINK_FLAGS) \
	    $(call SUBST,MKDLL) \
	    $(call SUBST,MKEXE) \
	    $(call SUBST,FLEXLINK_LDFLAGS) \
	    $(call SUBST,MKMAINDLL) \
	    $(call SUBST,MODEL) \
	    $(call SUBST,NATIVECCLIBS) \
	    $(call SUBST,OCAMLC_CFLAGS) \
	    $(call SUBST,OCAMLC_CPPFLAGS) \
	    $(call SUBST,OCAMLOPT_CFLAGS) \
	    $(call SUBST,OCAMLOPT_CPPFLAGS) \
	    $(call SUBST,PACKLD) \
	    $(call SUBST,PROFILING) \
	    $(call SUBST,PROFINFO_WIDTH) \
	    $(call SUBST,RANLIBCMD) \
	    $(call SUBST,FORCE_SAFE_STRING) \
	    $(call SUBST,DEFAULT_SAFE_STRING) \
	    $(call SUBST,WINDOWS_UNICODE) \
	    $(call SUBST,SYSTEM) \
	    $(call SUBST,SYSTHREAD_SUPPORT) \
	    $(call SUBST,TARGET) \
	    $(call SUBST,WITH_FRAME_POINTERS) \
	    $(call SUBST,WITH_PROFINFO) \
	    $(call SUBST,WITH_SPACETIME) \
	    $(call SUBST,ENABLE_CALL_COUNTS) \
	    $(call SUBST,FLAT_FLOAT_ARRAY) \
	    $< > $@

ifeq "$(UNIX_OR_WIN32)" "unix"
.PHONY: reconfigure
reconfigure:
	./configure $(CONFIGURE_ARGS)
endif

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml

.PHONY: beforedepend
beforedepend:: utils/config.ml

# Start up the system from the distribution compiler
.PHONY: coldstart
coldstart:
	$(MAKE) -C byterun $(BOOT_FLEXLINK_CMD) all
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C yacc $(BOOT_FLEXLINK_CMD) all
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) \
	  COMPILER="../boot/ocamlc -use-prims ../byterun/primitives" all
	cd stdlib; cp $(LIBFILES) ../boot
	cd boot; $(LN) ../byterun/libcamlrun.$(A) .

# Recompile the core system using the bootstrap compiler
.PHONY: coreall
coreall:
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamlyacc ocamltools library

# Build the core system: the minimum needed to make depend and bootstrap
.PHONY: core
core:
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MAKE) coldstart
else # Windows, to be fixed!
	$(MAKE) runtime
endif
	$(MAKE) coreall

# Save the current bootstrap compiler
.PHONY: backup
backup:
	$(MKDIR) boot/Saved
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/ocamlrun$(EXE) boot/Saved
	cd boot; mv ocamlc ocamllex ocamlyacc$(EXE) ocamldep Saved
	cd boot; cp $(LIBFILES) Saved

# Restore the saved bootstrap compiler if a problem arises
.PHONY: restore
restore:
	cd boot; mv Saved/* .; rmdir Saved; mv Saved.prev Saved

# Check if fixpoint reached
.PHONY: compare
compare:
	@if $(CAMLRUN) tools/cmpbyt boot/ocamlc ocamlc \
         && $(CAMLRUN) tools/cmpbyt boot/ocamllex lex/ocamllex \
         && $(CAMLRUN) tools/cmpbyt boot/ocamldep tools/ocamldep; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
.PHONY: promote-cross
promote-cross:
	$(CAMLRUN) tools/stripdebug ocamlc boot/ocamlc
	$(CAMLRUN) tools/stripdebug lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	$(CAMLRUN) tools/stripdebug tools/ocamldep boot/ocamldep
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
.PHONY: promote
promote: promote-cross
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Remove old bootstrap compilers
.PHONY: cleanboot
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Compile the native-code compiler
.PHONY: opt-core
opt-core: runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

.PHONY: opt
opt:
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt ocamltoolsopt
else
	$(MAKE) opt-core
	$(MAKE) otherlibrariesopt ocamltoolsopt
endif

# Native-code versions of the tools
.PHONY: opt.opt
ifeq "$(UNIX_OR_WIN32)" "unix"
opt.opt:
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
	  ocamltest.opt
else
opt.opt: core opt-core ocamlc.opt all ocamlopt.opt ocamllex.opt \
         ocamltoolsopt ocamltoolsopt.opt otherlibrariesopt $(OCAMLDOC_OPT) \
         ocamltest.opt
endif

.PHONY: base.opt
base.opt:
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt

# Core bootstrapping cycle
.PHONY: coreboot
coreboot:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# byterun/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on byterun/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex ocamltools
# Rebuild the library (using byterun/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) CAMLRUN=byterun/ocamlrun promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Recompile the system using the bootstrap compiler

.PHONY: all
all: runtime
	$(MAKE) coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
.PHONY: bootstrap
bootstrap: coreboot
	$(MAKE) all
	$(MAKE) compare

# Compile everything the first time

.PHONY: world
world: coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
.PHONY: world.opt
world.opt: coldstart
	$(MAKE) opt.opt

# FlexDLL sources missing error messages
# Different git mechanism displayed depending on whether this source tree came
# from a git clone or a source tarball.

flexdll/Makefile:
	@echo In order to bootstrap FlexDLL, you need to place the sources in
	@echo flexdll.
	@echo This can either be done by downloading a source tarball from
	@echo \  http://alain.frisch.fr/flexdll.html
	@if [ -d .git ]; then \
	  echo or by checking out the flexdll submodule with; \
	  echo \  git submodule update --init; \
	else \
	  echo or by cloning the git repository; \
	  echo \  git clone https://github.com/alainfrisch/flexdll.git; \
	fi
	@false

.PHONY: flexdll
flexdll: flexdll/Makefile flexlink
	$(MAKE) -C flexdll \
	     OCAML_CONFIG_FILE=../config/Makefile \
             MSVC_DETECT=0 CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false support

# Bootstrapping flexlink - leaves a bytecode image of flexlink.exe in flexdll/
.PHONY: flexlink
flexlink: flexdll/Makefile
	$(MAKE) -C byterun BOOTSTRAPPING_FLEXLINK=yes ocamlrun$(EXE)
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib COMPILER=../boot/ocamlc stdlib.cma std_exit.cmo
	cd stdlib && cp stdlib.cma std_exit.cmo *.cmi ../boot
	$(MAKE) -C flexdll MSVC_DETECT=0 OCAML_CONFIG_FILE=../config/Makefile \
	  CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false \
	  OCAMLOPT="../boot/ocamlrun ../boot/ocamlc -I ../boot" \
	  flexlink.exe
	$(MAKE) -C byterun clean
	$(MAKE) partialclean

.PHONY: flexlink.opt
flexlink.opt:
	cd flexdll && \
	mv flexlink.exe flexlink && \
	$(MAKE) OCAML_FLEXLINK="../boot/ocamlrun ./flexlink" MSVC_DETECT=0 \
	           OCAML_CONFIG_FILE=../config/Makefile \
	           OCAMLOPT="../ocamlopt.opt -I ../stdlib" flexlink.exe && \
	mv flexlink.exe flexlink.opt && \
	mv flexlink flexlink.exe

.PHONY: install-flexdll
install-flexdll:
	cat stdlib/camlheader flexdll/flexlink.exe > \
	  "$(INSTALL_BINDIR)/flexlink.exe"
ifneq "$(filter-out mingw,$(TOOLCHAIN))" ""
	cp flexdll/default$(filter-out _i386,_$(ARCH)).manifest \
    "$(INSTALL_BINDIR)/"
endif
	if test -n "$(wildcard flexdll/flexdll_*.$(O))" ; then \
	  $(MKDIR) "$(INSTALL_FLEXDLL)" ; \
	  cp flexdll/flexdll_*.$(O) "$(INSTALL_FLEXDLL)" ; \
	fi

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	cp VERSION "$(INSTALL_LIBDIR)"
	$(MAKE) -C byterun install
	cp ocaml "$(INSTALL_BINDIR)/ocaml$(EXE)"
	cp ocamlc "$(INSTALL_BINDIR)/ocamlc.byte$(EXE)"
	$(MAKE) -C stdlib install
	cp lex/ocamllex "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
	cp yacc/ocamlyacc$(EXE) "$(INSTALL_BINDIR)/ocamlyacc$(EXE)"
	cp utils/*.cmi utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmi parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmi typing/*.cmt typing/*.cmti typing/*.mli \
	   bytecomp/*.cmi bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmi driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmi toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
	cp compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
	   compilerlibs/ocamltoplevel.cma $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	cp expunge "$(INSTALL_LIBDIR)/expunge$(EXE)"
	cp toplevel/topdirs.cmi toplevel/topdirs.cmt toplevel/topdirs.cmti \
           toplevel/topdirs.mli "$(INSTALL_LIBDIR)"
	$(MAKE) -C tools install
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MKDIR) "$(INSTALL_MANDIR)/man$(PROGRAMS_MAN_SECTION)"
	-$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
# Transitional: findlib 1.7.3 is confused if leftover num.cm? files remain
# from an previous installation of OCaml before otherlibs/num was removed.
	rm -f "$(INSTALL_LIBDIR)"/num.cm?
# End transitional
	if test -n "$(WITH_OCAMLDOC)"; then \
	  $(MAKE) -C ocamldoc install; \
	fi
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(MAKE) -C debugger install; \
	fi
ifeq "$(UNIX_OR_WIN32)" "win32"
	if test -n "$(FLEXDLL_SUBMODULE_PRESENT)"; then \
	  $(MAKE) install-flexdll; \
	fi
endif
	cp config/Makefile "$(INSTALL_LIBDIR)/Makefile.config"
	if test -f ocamlopt; then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi

# Installation of the native-code compiler
.PHONY: installopt
installopt:
	$(MAKE) -C asmrun install
	cp ocamlopt "$(INSTALL_BINDIR)/ocamlopt.byte$(EXE)"
	$(MAKE) -C stdlib installopt
	cp middle_end/*.cmi middle_end/*.cmt middle_end/*.cmti \
	    middle_end/*.mli \
		"$(INSTALL_COMPLIBDIR)"
	cp middle_end/base_types/*.cmi middle_end/base_types/*.cmt \
	    middle_end/base_types/*.cmti middle_end/base_types/*.mli \
		"$(INSTALL_COMPLIBDIR)"
	cp asmcomp/*.cmi asmcomp/*.cmt asmcomp/*.cmti asmcomp/*.mli \
		"$(INSTALL_COMPLIBDIR)"
	cp compilerlibs/ocamloptcomp.cma $(OPTSTART) "$(INSTALL_COMPLIBDIR)"
	if test -n "$(WITH_OCAMLDOC)"; then \
	  $(MAKE) -C ocamldoc installopt; \
	fi
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i installopt || exit $$?; \
	done
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.byte$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi
	$(MAKE) -C tools installopt
	if test -f ocamlopt.opt -a -f flexdll/flexlink.opt ; then \
	  cp -f flexdll/flexlink.opt "$(INSTALL_BINDIR)/flexlink$(EXE)" ; \
	fi

.PHONY: installoptopt
installoptopt:
	cp ocamlc.opt "$(INSTALL_BINDIR)/ocamlc.opt$(EXE)"
	cp ocamlopt.opt "$(INSTALL_BINDIR)/ocamlopt.opt$(EXE)"
	cp lex/ocamllex.opt "$(INSTALL_BINDIR)/ocamllex.opt$(EXE)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
	cp utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   driver/*.cmx asmcomp/*.cmx "$(INSTALL_COMPLIBDIR)"
	cp compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.$(A) \
	   compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.$(A) \
	   compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.$(A) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
	if test -f ocamlnat$(EXE) ; then \
	  cp ocamlnat$(EXE) "$(INSTALL_BINDIR)/ocamlnat$(EXE)"; \
	  cp toplevel/opttopdirs.cmi "$(INSTALL_LIBDIR)"; \
	  cp compilerlibs/ocamlopttoplevel.cmxa \
	     compilerlibs/ocamlopttoplevel.$(A) \
	     $(OPTTOPLEVELSTART:.cmo=.cmx) $(OPTTOPLEVELSTART:.cmo=.$(O)) \
	     "$(INSTALL_COMPLIBDIR)"; \
	fi
	cd "$(INSTALL_COMPLIBDIR)" && \
	   $(RANLIB) ocamlcommon.$(A) ocamlbytecomp.$(A) ocamloptcomp.$(A)

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
	cp utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
	   toplevel/*.ml middle_end/*.ml middle_end/base_types/*.ml \
	   asmcomp/*.ml $(INSTALL_COMPLIBDIR)

# Run all tests

.PHONY: tests
tests: opt.opt ocamltest
	cd testsuite; $(MAKE) clean && $(MAKE) all

# Make clean in the test suite

.PHONY: clean
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

# The clean target
clean:: partialclean

# Shared parts of the system

compilerlibs/ocamlcommon.cma: $(COMMON)
	$(CAMLC) -a -linkall -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlcommon.cma

# The bytecode compiler

compilerlibs/ocamlbytecomp.cma: $(BYTECOMP)
	$(CAMLC) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cma

ocamlc: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma $(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc

# The native-code compiler

compilerlibs/ocamloptcomp.cma: $(MIDDLE_END) $(ASMCOMP)
	$(CAMLC) -a -o $@ $^

partialclean::
	rm -f compilerlibs/ocamloptcomp.cma

ocamlopt: compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt

# The toplevel

compilerlibs/ocamltoplevel.cma: $(TOPLEVEL)
	$(CAMLC) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamltoplevel.cma

ocaml_dependencies := \
  compilerlibs/ocamlcommon.cma \
  compilerlibs/ocamlbytecomp.cma \
  compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART)

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: $(ocaml_dependencies)
	$(CAMLC) $(LINKFLAGS) -linkall -o $@ $^

ocaml: expunge ocaml.tmp
	- $(CAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml

.PHONY: runtop
runtop:
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) ocaml
else
	$(MAKE) core
	$(MAKE) ocaml
endif
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(RUNTOP) ||\
	  $(EXTRAPATH) $(RUNTOP)

.PHONY: natruntop
natruntop:
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) opt.opt
	$(MAKE) ocamlnat
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(NATRUNTOP) ||\
	  $(EXTRAPATH) $(NATRUNTOP)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) $<

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) $<

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# Shared parts of the system compiled with the native-code compiler

compilerlibs/ocamlcommon.cmxa: $(COMMON:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.$(A)

# The bytecode compiler compiled with the native-code compiler

compilerlibs/ocamlbytecomp.cmxa: $(BYTECOMP:.cmo=.cmx)
	$(CAMLOPT) -a $(OCAML_NATDYNLINKOPTS) -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.$(A)

ocamlc.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlbytecomp.cmxa \
            $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

compilerlibs/ocamloptcomp.cmxa: $(MIDDLE_END:.cmo=.cmx) $(ASMCOMP:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.$(A)

ocamlopt.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
              $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt

$(COMMON:.cmo=.cmx) $(BYTECOMP:.cmo=.cmx) $(MIDDLE_END:.cmo=.cmx) \
$(ASMCOMP:.cmo=.cmx): ocamlopt

# The predefined exceptions and primitives

byterun/primitives:
	$(MAKE) -C byterun primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/caml/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 cat byterun/caml/fail.h | tr -d '\r' | \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p'; \
	 echo '|]'; \
	 echo 'let builtin_primitives = [|'; \
	 sed -e 's/.*/  "&";/' byterun/primitives; \
	 echo '|]') > $@

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	echo \# 1 \"$(ARCH)/emit.mlp\" > $@
	$(CAMLRUN) tools/cvt_emit < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	$(MAKE) -C tools cvt_emit

# The "expunge" utility

expunge: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime:
	$(MAKE) -C byterun $(BOOT_FLEXLINK_CMD) all
byterun/libcamlrun.$(A): makeruntime ;
stdlib/libcamlrun.$(A): byterun/libcamlrun.$(A)
	cd stdlib; $(LN) ../byterun/libcamlrun.$(A) .
clean::
	$(MAKE) -C byterun clean
	rm -f stdlib/libcamlrun.$(A)

otherlibs_all := bigarray dynlink graph raw_spacetime_lib \
  str systhreads threads unix win32graph win32unix
subdirs := asmrun byterun debugger lex ocamldoc ocamltest stdlib tools \
  $(addprefix otherlibs/, $(otherlibs_all))

.PHONY: alldepend
ifeq "$(TOOLCHAIN)" "msvc"
alldepend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done
endif

# The runtime system for the native-code compiler

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt:
	$(MAKE) -C asmrun $(BOOT_FLEXLINK_CMD) all
asmrun/libasmrun.$(A): makeruntimeopt ;
stdlib/libasmrun.$(A): asmrun/libasmrun.$(A)
	cp $< $@
clean::
	$(MAKE) -C asmrun clean
	rm -f stdlib/libasmrun.$(A)

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) CAMLRUN=../byterun/ocamlrun all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer and parser generators

.PHONY: ocamllex
ocamllex: ocamlyacc ocamlc
	$(MAKE) -C lex all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) -C lex allopt

partialclean::
	$(MAKE) -C lex clean

.PHONY: ocamlyacc
ocamlyacc:
	$(MAKE) -C yacc $(BOOT_FLEXLINK_CMD) all

clean::
	$(MAKE) -C yacc clean

# OCamldoc

.PHONY: ocamldoc
ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C ocamldoc all

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamldoc opt.opt

# OCamltest
ocamltest: ocamlc ocamlyacc ocamllex
	$(MAKE) -C ocamltest

ocamltest.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamltest ocamltest.opt$(EXE)

partialclean::
	$(MAKE) -C ocamltest clean

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C ocamldoc $@
	@echo "documentation is in ./ocamldoc/stdlib_html/"

partialclean::
	$(MAKE) -C ocamldoc clean

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools
	for i in $(OTHERLIBRARIES); do \
	  ($(MAKE) -C otherlibs/$$i all) || exit $$?; \
	done

.PHONY: otherlibrariesopt
otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
	  ($(MAKE) -C otherlibs/$$i allopt) || exit $$?; \
	done

partialclean::
	for i in $(OTHERLIBRARIES); do \
	  ($(MAKE) -C otherlibs/$$i partialclean); \
	done

clean::
	for i in $(OTHERLIBRARIES); do \
	  ($(MAKE) -C otherlibs/$$i clean); \
	done

# The replay debugger

.PHONY: ocamldebugger
ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C debugger all

partialclean::
	$(MAKE) -C debugger clean

# Check that the stack limit is reasonable.
ifeq "$(UNIX_OR_WIN32)" "unix"
.PHONY: checkstack
checkstack:
	if $(MKEXE) $(OUTPUTEXE)tools/checkstack$(EXE) tools/checkstack.c; \
	  then tools/checkstack$(EXE); \
	  else :; \
	fi
	rm -f tools/checkstack$(EXE)
endif

# Lint @since and @deprecated annotations

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff:
	$(MAKE) -C tools lintapidiff.opt
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|spacetime\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# Make clean in the test suite

clean::
	cd testsuite; $(MAKE) clean

# Make MacOS X package
ifeq "$(UNIX_OR_WIN32)" "unix"
.PHONY: package-macosx
package-macosx:
	sudo rm -rf package-macosx/root
	$(MAKE) PREFIX="`pwd`"/package-macosx/root install
	tools/make-package-macosx
	sudo rm -rf package-macosx/root

clean::
	rm -rf package-macosx/*.pkg package-macosx/*.dmg
endif

# The middle end (whose .cma library is currently only used for linking
# the "ocamlobjinfo" program, since we cannot depend on the whole native code
# compiler for "make world" and the list of dependencies for
# asmcomp/export_info.cmo is long).

compilerlibs/ocamlmiddleend.cma: $(MIDDLE_END)
	$(CAMLC) -a -o $@ $^
compilerlibs/ocamlmiddleend.cmxa: $(MIDDLE_END:%.cmo=%.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlmiddleend.cma \
	      compilerlibs/ocamlmiddleend.cmxa \
	      compilerlibs/ocamlmiddleend.$(A)

# Tools

.PHONY: ocamltools
ocamltools: ocamlc ocamlyacc ocamllex asmcomp/cmx_format.cmi \
            asmcomp/printclambda.cmo compilerlibs/ocamlmiddleend.cma \
            asmcomp/export_info.cmo
	$(MAKE) -C tools all

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) -C tools opt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamlyacc ocamllex.opt asmcomp/cmx_format.cmi \
                   asmcomp/printclambda.cmx compilerlibs/ocamlmiddleend.cmxa \
                   asmcomp/export_info.cmx
	$(MAKE) -C tools opt.opt

partialclean::
	$(MAKE) -C tools clean

## Test compilation of backend-specific parts

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS

# Compiler Plugins

DYNLINK_DIR=otherlibs/dynlink

driver/compdynlink.mlbyte: $(DYNLINK_DIR)/dynlink.ml driver/compdynlink.mli
	grep -v 'REMOVE_ME for ../../debugger/dynlink.ml' \
	     $(DYNLINK_DIR)/dynlink.ml >driver/compdynlink.mlbyte

ifeq ($(NATDYNLINK),true)
driver/compdynlink.mlopt: $(DYNLINK_DIR)/natdynlink.ml driver/compdynlink.mli
	cp $(DYNLINK_DIR)/natdynlink.ml driver/compdynlink.mlopt
else
driver/compdynlink.mlopt: driver/compdynlink.mlno driver/compdynlink.mli
	cp driver/compdynlink.mlno driver/compdynlink.mlopt
endif

driver/compdynlink.mli: $(DYNLINK_DIR)/dynlink.mli
	cp $(DYNLINK_DIR)/dynlink.mli driver/compdynlink.mli

driver/compdynlink.cmo: driver/compdynlink.mlbyte driver/compdynlink.cmi
	$(CAMLC) $(COMPFLAGS) -c -impl $<

driver/compdynlink.cmx: driver/compdynlink.mlopt driver/compdynlink.cmi
	$(CAMLOPT) $(COMPFLAGS) -c -impl $<

beforedepend:: driver/compdynlink.mlbyte driver/compdynlink.mlopt \
               driver/compdynlink.mli
partialclean::
	rm -f driver/compdynlink.mlbyte
	rm -f driver/compdynlink.mli
	rm -f driver/compdynlink.mlopt

# The native toplevel

compilerlibs/ocamlopttoplevel.cmxa: $(OPTTOPLEVEL:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlopttoplevel.cmxa

# When the native toplevel executable has an extension (e.g. ".exe"),
# provide a phony 'ocamlnat' synonym

ifneq ($(EXE),)
.PHONY: ocamlnat
ocamlnat: ocamlnat$(EXE)
endif

ocamlnat$(EXE): compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
    compilerlibs/ocamlbytecomp.cmxa \
    compilerlibs/ocamlopttoplevel.cmxa \
    $(OPTTOPLEVELSTART:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -linkall -o $@ $^

partialclean::
	rm -f ocamlnat$(EXE)

toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: byterun/caml/instruct.h tools/make_opcodes
	$(CAMLRUN) tools/make_opcodes -opcodes < $< > $@

tools/make_opcodes: tools/make_opcodes.mll
	$(MAKE) -C tools make_opcodes

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end \
	         middle_end/base_types asmcomp/debug driver toplevel tools; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.$(S) \
	    $$d/*.$(O) $$d/*.$(SO) $d/*~; \
	done
	rm -f *~

.PHONY: depend
depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp middle_end \
	 middle_end/base_types asmcomp/debug driver toplevel; \
	 do $(CAMLDEP) -slash $(DEPFLAGS) $$d/*.mli $$d/*.ml || exit; \
	 done) > .depend
	$(CAMLDEP) -slash $(DEPFLAGS) -native \
		-impl driver/compdynlink.mlopt >> .depend
	$(CAMLDEP) -slash $(DEPFLAGS) -bytecode \
		-impl driver/compdynlink.mlbyte >> .depend

.PHONY: distclean
distclean: clean
	rm -f boot/ocamlrun boot/ocamlrun$(EXE) boot/camlheader \
	      boot/ocamlyacc boot/*.cm* boot/libcamlrun.$(A)
	rm -f config/Makefile byterun/caml/m.h byterun/caml/s.h
	rm -f tools/*.bak
	rm -f ocaml ocamlc
	rm -f testsuite/_log

include .depend
