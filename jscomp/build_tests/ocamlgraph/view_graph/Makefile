OCAMLC=ocamlc.opt -dtypes 
OCAMLOPT=ocamlopt.opt
OCAMLDOC=ocamldoc.opt

GRAPH_DIR=..
GRAPH_LIB=graph.cmo

INCLUDES=-I . -I +lablgtk2 -I $(GRAPH_DIR)

FILES = viewGraph viewGraph_select viewGraph_utils viewGraph_test
SRCS = $(wildcard *.ml *.mli)
CMOS = $(FILES:%=%.cmo)
CMXS = $(FILES:%=%.cmx)

EXEC=viewgraph

all : $(EXEC) $(CMXS) doc

$(EXEC) :  $(CMOS) $(GRAPH_DIR)/$(GRAPH_LIB)
	$(OCAMLC) $(INCLUDES) -o $@ \
	  unix.cma lablgtk.cma lablgnomecanvas.cma $(GRAPH_LIB) \
	  $(CMOS)

depend: .depend

.depend : $(SRCS)
	ocamldep $(SRCS) > $@

include .depend

.PHONY: doc
doc : $(CMOS) doc/intro.txt doc/style.css
	$(OCAMLDOC) -d doc \
	  -stars -html -intro doc/intro.txt -css-style style.css \
	  $(INCLUDES) $(SRCS)

%.cmo :%.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmx :%.ml
	$(OCAMLOPT) $(INCLUDES) -c $<

%.cmi :%.mli
	$(OCAMLC) $(INCLUDES) -c $<

clean :
	rm -f $(EXEC) 
	rm -f .depend 
	rm -f $(FILES:%=%.cmo) $(FILES:%=%.cmx) $(FILES:%=%.o) $(FILES:%=%.cmi) $(FILES:%=%.annot)
	rm -f doc/*.html
