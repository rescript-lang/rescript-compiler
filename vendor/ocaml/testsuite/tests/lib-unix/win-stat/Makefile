BASEDIR=../../..
LIBRARIES=unix
ADD_COMPFLAGS=-I $(OTOPDIR)/otherlibs/$(UNIXLIBVAR)unix
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix
C_FILES=fakeclock
MAIN_MODULE=test
TEST_TEMP_FILES=dst-file non-dst-file

ifeq ($(OS),Windows_NT)
test:
	@TZ=utc touch -m -t 201707011200 dst-file
	@TZ=utc touch -m -t 201702011200 non-dst-file
	@$(MAKE) default
else
skip:
	@echo " ... testing => skipped (not on Windows)"
endif

include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common
