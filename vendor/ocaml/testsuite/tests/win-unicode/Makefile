BASEDIR=../..
LIBRARIES=unix
ADD_COMPFLAGS= \
	-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix \
	-strict-sequence -safe-string -w A -warn-error A
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
C_FILES=mkfiles

.PHONY: test
test:
	@if echo 'let () = exit (if Config.windows_unicode then 0 else 1)' | $(OCAML) -I $(OTOPDIR)/utils config.cmo -stdin; then \
	  $(MAKE) printargv.exe printenv.exe symlink_tests.precheck && \
	  $(MAKE) check; \
	else \
	  $(MAKE) SKIP=true C_FILES= run-all; \
	fi

.PHONY: symlink_tests.precheck
symlink_tests.precheck:
	@echo 'echo "let () = exit (if Unix.has_symlink () then 0 else 1)" | $(OCAML) $(ADD_COMPFLAGS) unix.cma -stdin' > $@

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES=symlink_tests.precheck

%.exe: %.c
	@$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(if $(filter msvc,$(CCOMPTYPE)),/Fe$*.exe,-o$*.exe) $*.c
