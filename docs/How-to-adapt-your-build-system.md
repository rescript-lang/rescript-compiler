The BuckleScript compilation model is the same as the OCaml compiler. 

If `b.ml` depends on `a.ml`, you have to compile `a.ml` **and** `a.mli` first. 

> The technical reason is that BuckleScript will generate intermediate files with the extension `.cmj` which are later used for cross module inlining and other information. 

Here is a simple Makefile to get started:

```make
OCAMLC=bsc
  # bsc is BuckleScript compiler
OCAMLDEP=ocamldep               
  # ocamldep executable is part of the OCaml compiler installation                                                                                                                                                                                                                                                                                                                                                                 

SOURCE_LIST := src_a src_b                                                                                                                                                                                                  
SOURCE_MLI  = $(addsuffic .mli, $(SOURCE_LIST))
SOURCE_ML   = $(addsuffic .ml, $(SOURCE_LIST))

TARGETS := $(addsuffix .cmj, $(SOURCE_LIST))                                                                                                                                                                                                                                                                                                                                                        

INCLUDES=

all: $(TARGETS)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      

.mli.cmi:                                                                                                                                                                                         
        $(OCAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<                                                                                                                                                  
.ml.cmj:                                                                                                                                                                                          
        $(OCAMLC) $(INCLUDES) $(COMPFLAGS)  -c $<                                                                                                                                                                                                                                                                                                                                                    

-include .depend                                                                                                                                                                                                                                                                                                                                                                                    

depend:                                                                                                                                                                                           
        $(OCAMLDEP) $(INCLUDES) $(SOURCE_ML) $(SOURCE_MLI) | sed -e 's/\.cmx/.cmj/g' > .depend    
```
