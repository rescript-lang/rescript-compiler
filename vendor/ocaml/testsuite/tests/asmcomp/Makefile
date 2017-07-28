#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..

CC=$(NATIVECC)
CFLAGS=$(NATIVECCCOMPOPTS) -g

INCLUDES=\
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/typing \
  -I $(OTOPDIR)/bytecomp \
  -I $(OTOPDIR)/asmcomp

OTHEROBJS=\
  $(OTOPDIR)/compilerlibs/ocamlcommon.cma \
  $(OTOPDIR)/compilerlibs/ocamloptcomp.cma

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

default:
	@if $(BYTECODE_ONLY) || [ -z "$(ASPP)" ]; then : ; else \
	  $(MAKE) all; \
	fi

all:
	$(MAKE) arch codegen
	$(MAKE) tests

codegen: parsecmm.ml lexcmm.ml $(OBJS:.cmo=.cmi) $(OBJS) main.cmo
	@$(OCAMLC) $(LINKFLAGS) -o codegen $(OTHEROBJS) $(OBJS) main.cmo

parsecmm.mli parsecmm.ml: parsecmm.mly
	@$(OCAMLYACC) -q parsecmm.mly

lexcmm.ml: lexcmm.mll
	@$(OCAMLLEX) -q lexcmm.mll

MLCASES=optargs staticalloc

CASES=fib tak quicksort quicksort2 soli \
      arith checkbound tagged-fib tagged-integr tagged-quicksort tagged-tak
ARGS_fib=-DINT_INT -DFUN=fib main.c
ARGS_tak=-DUNIT_INT -DFUN=takmain main.c
ARGS_quicksort=-DSORT -DFUN=quicksort main.c
ARGS_quicksort2=-DSORT -DFUN=quicksort main.c
ARGS_soli=-DUNIT_INT -DFUN=solitaire main.c
ARGS_integr=-DINT_FLOAT -DFUN=test main.c
ARGS_arith=mainarith.c
ARGS_checkbound=-DCHECKBOUND main.c
ARGS_tagged-fib=-DINT_INT -DFUN=fib main.c
ARGS_tagged-integr=-DINT_FLOAT -DFUN=test main.c
ARGS_tagged-quicksort=-DSORT -DFUN=quicksort main.c
ARGS_tagged-tak=-DUNIT_INT -DFUN=takmain main.c

tests: $(CASES:=.o)
	@for c in $(CASES); do \
	  printf " ... testing '$$c':"; \
	  $(MAKE) one CC="$(CC) $(CFLAGS)" NAME=$$c; \
	done
	@for c in $(MLCASES); do \
	  printf " ... testing '$$c':"; \
          $(MAKE) one_ml NAME=$$c; \
        done

one_ml:
	@$(OCAMLOPT) -o $(NAME).exe $(NAME).ml && \
	./$(NAME).exe && echo " => passed" || echo " => failed"

one:
	@$(CC) -o $(NAME).out $(ARGS_$(NAME)) $(NAME).o $(ARCH).o \
	&& echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f ./codegen *.out *.o *.obj *.exe
	@rm -f parsecmm.ml parsecmm.mli lexcmm.ml
	@rm -f $(CASES:=.s)

include $(BASEDIR)/makefiles/Makefile.common

power.o: power-$(SYSTEM).o
	@cp power-$(SYSTEM).o power.o

promote:

arch: $(ARCH).o
