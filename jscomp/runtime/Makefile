include ../Makefile.shared

COMPILER=../bin/bsc

SOURCE_LIST := $(shell cat runtime.mllib)

RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST))


COMPFLAGS += $(MODULE_FLAGS) -w -40 



$(RUNTIME): $(COMPILER)

all: $(RUNTIME)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map


.mli.cmi:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmj:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<

-include .runtimedepend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's/\.cmo/.cmj/g' > .runtimedepend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >>.runtimedepend

