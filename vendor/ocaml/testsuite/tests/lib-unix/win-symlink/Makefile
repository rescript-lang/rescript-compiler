BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
MAIN_MODULE=test
TEST_TEMP_FILES=link1 link2 test.txt

test:
	@if $(OCAML) $(ADD_COMPFLAGS) unix.cma precheck.ml; then \
	  $(MAKE) default; \
	else \
	  echo " ... testing => skipped (not on Windows and/or symlinks not allowed)"; \
	fi

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
