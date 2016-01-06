CAMLDEP=ocamldep.opt
CAMLLEX=ocamllex.opt
CAMLC=../bin/ocamlscript

CAMLOPT=
INCLUDES= -I ../stdlib -I ../runtime


SOURCE_LIST := $(shell cat test.mllib)
TESTS := $(addsuffix .cmo, $(SOURCE_LIST) )

COMPFLAGS+= -safe-string -w -40 

$(TESTS): $(CAMLC)

all: $(TESTS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx .p.cmx

TEST_JS = $(TESTS:.cmo=.js)

clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f $(TEST_JS)
	rm -f *.rawlambda *.lam *.lambda *.map
	rm arith_lexer.ml number_lexer.ml simple_lexer_test.ml

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) `./Compflags $@` -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) `./Compflags $@` -c $<

.ml.cmx:
	$(CAMLOPT) $(INCLUDES)  $(COMPFLAGS) `./Compflags $@` -c $<

.ml.p.cmx:
	$(CAMLOPT) $(INCLUDES) $(COMPFLAGS) `./Compflags $@` -p -c -o $*.p.cmx $<

arith_lexer.ml: arith_lexer.mll
	$(CAMLLEX) $<
number_lexer.ml: number_lexer.mll
	$(CAMLLEX) $<
simple_lexer_test.ml: simple_lexer_test.mll
	$(CAMLLEX) $<

-include .depend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmo/g' >>.depend

