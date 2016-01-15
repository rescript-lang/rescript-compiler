include ../Makefile.shared

CAMLC=../bin/osc
# INCLUDES= -I ../stdlib
SOURCE_LIST := $(shell cat runtime.mllib)

RUNTIME := $(addsuffix .cmo, $(SOURCE_LIST))

$(RUNTIME): $(CAMLC)

all: $(RUNTIME)



.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<

-include .depend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmo/g' >>.depend

