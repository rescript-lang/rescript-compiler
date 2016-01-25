CAMLDEP=ocamldep.opt
CAMLLEX=ocamllex.opt
CAMLC=../bin/osc

CAMLOPT=
INCLUDES= -I ../stdlib -I ../runtime


SOURCE_LIST := $(shell cat test.mllib)
TESTS := $(addsuffix .cmo, $(SOURCE_LIST) )

COMPFLAGS+= -safe-string -w -40 

$(TESTS): $(CAMLC)

all: $(TESTS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx .cmj

TEST_JS = $(TESTS:.cmo=.cmj)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f $(TEST_JS)
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
	$(CAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >>.depend

