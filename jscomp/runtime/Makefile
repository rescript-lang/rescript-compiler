include ../Makefile.shared

CAMLC=../bin/osc
# INCLUDES= -I ../stdlib
SOURCE_LIST := $(shell cat runtime.mllib)

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))

$(RUNTIME): $(CAMLC)

all: $(RUNTIME)



.SUFFIXES: .mli .ml .cmi .cmj .cmx .p.cmx .js

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

