include ../Makefile.shared

CAMLC=../bin/bsc.exe
BSPACK=../bin/bspack.exe
INCLUDES= -I ../stdlib  -I ../runtime -I ../others/

OTHERS := xwatcher xwatcher_current xwatcher_util

SOURCE_LIST :=  $(OTHERS)
TESTS := $(addsuffix .cmj, $(SOURCE_LIST) )
## since we use ppx
$(addsuffix .cmi, $(OTHERS)): ../runtime/js.cmi
$(addsuffix .cmj, $(OTHERS)): ../runtime/js.cmj

# It is okay to test deprecated API
COMPFLAGS+=  $(MODULE_FLAGS) -w -40 -warn-error A+8-3-30-26
COMPFLAGS+=  -bs-no-version-header -absname -bs-diagnose -bs-cross-module-opt -bs-package-name  bs-platform -bs-package-output es6:jscomp/xwatcher


$(TESTS): $(CAMLC)

all: $(TESTS)

bundle: all
	rollup --silent -f cjs xwatcher_current.js -o ../../scripts/watcher.js
	# rollup --silent -f cjs xwatcher.js -o ../bin/bsb_watcher.future.js
clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f $(TESTS)
	rm -f *.rawlambda *.lam *.lambda *.map


.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmj:
	$(CAMLC) $(INCLUDES)  $(COMPFLAGS) -c $<



-include .depend

depend:
	$(CAMLDEP) -native $(INCLUDES) *.mli *.ml | sed -e 's/\.cmx/.cmj/g' > .depend
