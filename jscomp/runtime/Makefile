include ../Makefile.shared

COMPILER=../../lib/bsc.exe

BS_COMMON_FLAGS= -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform

BS_FLAGS=  $(BS_COMMON_FLAGS) $(BS_PKG_FLAGS)

COMPFLAGS += $(BS_FLAGS)  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map
.mli.cmi:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) -open Bs_stdlib_mini $(INCLUDES) $(COMPFLAGS)  -c $<

bs_stdlib_mini.cmi:	bs_stdlib_mini.mli
	$(COMPILER) $(COMPFLAGS) -c $<

# This file no longer works
# include .depend

	


