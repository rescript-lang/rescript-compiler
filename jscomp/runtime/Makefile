include ../Makefile.shared

CAMLC=../bin/bsc

SOURCE_LIST := $(shell cat runtime.mllib)

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))

# COMPFLAGS := -js-module amdjs
COMPFLAGS += -w -40 -safe-string 
# COMPFLAGS += -js-module goog:buckle.runtime


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
	$(CAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >>.depend

