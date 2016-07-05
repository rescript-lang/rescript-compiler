include ../Makefile.shared

CAMLC=../bin/bsc

INCLUDES= -I ../stdlib  -I ../runtime

SOURCE_LIST := $(shell cat test.mllib)
TESTS := $(addsuffix .cmj, $(SOURCE_LIST) )
## since we use ppx
$(addsuffix .cmi, $(SOURCE_LIST)): ../runtime/js.cmi

COMPFLAGS+=  $(MODULE_FLAGS) -w -40 
COMPFLAGS+=  -bs-diagnose -bs-cross-module-opt -bs-npm-output-path $(npm_package_name):lib/js/test/


$(TESTS): $(CAMLC)

all: $(TESTS)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f $(TESTS)
	rm -f *.rawlambda *.lam *.lambda *.map
	rm arith_lexer.ml number_lexer.ml simple_lexer_test.ml

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmj:
	$(CAMLC) $(INCLUDES)  $(COMPFLAGS) -c $<


arith_lexer.ml: arith_lexer.mll
	$(CAMLLEX) $<
number_lexer.ml: number_lexer.mll
	$(CAMLLEX) $<
simple_lexer_test.ml: simple_lexer_test.mll
	$(CAMLLEX) $<

-include .depend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's/\.cmo/.cmj/g' > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >> .depend


