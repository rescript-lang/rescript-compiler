include ../Makefile.shared

CAMLC=../bin/bsc

SOURCE_LIST := $(shell cat lib.mllib)

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))
# COMPFLAGS += -js-module goog:buckle.js

$(RUNTIME): $(CAMLC)

all: $(RUNTIME)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map


.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<

-include .depend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's/\.cmx/.cmj/g' > .depend

