BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS= \
	-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix \
	-strict-sequence -safe-string -w A -warn-error A
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
C_FILES=stubs

.PHONY: test
test:
	@if echo 'let () = exit (if Sys.win32 then 0 else 1)' | $(OCAML) -stdin; then \
	  $(MAKE) check; \
	else \
	  $(MAKE) SKIP=true C_FILES= run-all; \
	fi

include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common
