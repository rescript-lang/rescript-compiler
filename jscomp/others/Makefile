include ../Makefile.shared

COMPILER=../bin/bsc

SOURCE_LIST= js_dyn


RUNTIME := $(addsuffix .cmj, $(SOURCE_LIST)) 

BS_COMMON_FLAGS= -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name $(npm_package_name)

ifdef BS_RELEASE_BUILD
BS_FLAGS=  $(BS_COMMON_FLAGS) -bs-package-output lib/js -bs-package-output goog:lib/goog -bs-package-output amdjs:lib/amdjs
else
BS_FLAGS=  $(BS_COMMON_FLAGS) -bs-package-output lib/js 
endif

COMPFLAGS += $(BS_FLAGS)  -I ../runtime -I ../stdlib -w -40



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

-include .depend

depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's/\.cmo/.cmj/g' > .depend
	$(CAMLDEP) $(INCLUDES) *.ml | sed -e 's/\.cmx/.cmj/g' >>.depend

