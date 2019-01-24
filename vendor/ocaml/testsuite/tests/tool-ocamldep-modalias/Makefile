# Test for ocamldep and -no-alias-deps
# There are two versions:
#  Makefile.build uses -no-alias-deps only for lib.ml/mli
#  Makefile.build2 has no lib.ml, and uses -no-alias-deps for components too

OCAMLDEP=$(OCAMLRUN) $(OTOPDIR)/tools/ocamldep
SOURCES = A.ml B.ml C.ml D.ml
LINKS = $(SOURCES:%=Lib%)
DEPENDS = depend.mk depend.mk2 depend.mod depend.mod2 depend.mod3

all:	clean
	@$(MAKE) build > /dev/null
	@$(MAKE) $(DEPENDS) > /dev/null
	@$(MAKE) compare

build:	depend.mk depend.mk2
	rm -f $(LINKS)
	if $(NATIVECODE_ONLY); then : ; else \
	   $(MAKE) -f Makefile.build byte; \
	   rm -f *.cm* lib.ml; \
	   $(MAKE) -f Makefile.build2 byte; fi
	if $(BYTECODE_ONLY); then :; else \
	   $(MAKE) -f Makefile.build opt; \
	   rm -f *.cm* lib.ml; \
	   $(MAKE) -f Makefile.build2 opt; fi

# Create links for prefixed versions of the components
Lib%.ml: %.ml
	cp $< $@

# Dependencies for Makefile.build, compiling and linking lib.cmo
depend.mk: $(LINKS)
	cp lib_impl.ml lib.ml
	$(OCAMLDEP) -as-map lib.ml lib.mli > $@
	$(OCAMLDEP) -map lib.ml -open Lib $(LINKS) >> $@

# Dependencies for Makefile.build2, not compiling lib.cmo
depend.mk2: $(LINKS)
	rm -f lib.ml
	$(OCAMLDEP) -map lib.mli -open Lib \
	  $(LINKS) > $@

# Others tests for ocamldep
depend.mod: $(LINKS)
	cp lib_impl.ml lib.ml
	$(OCAMLDEP) -as-map -modules lib.ml lib.mli > $@
	$(OCAMLDEP) -modules -map lib.ml -open Lib $(LINKS) >> $@

depend.mod2: $(LINKS)
	rm -f lib.ml
	$(OCAMLDEP) -modules -map lib.mli $(LINKS) > $@

depend.mod3: $(LINKS)
	rm -f lib.ml
	$(OCAMLDEP) -modules -as-map -map lib.mli -open Lib \
	  $(LINKS) > $@

promote:
	for i in $(DEPENDS); do cp $$i $$i.reference; done

compare: $(DEPENDS)
	@rm -f $(LINKS) lib.ml
	@for i in $(DEPENDS); do \
	  printf " ... testing '$$i':"; \
	  $(DIFF) $$i.reference $$i > /dev/null \
          && echo " => passed" || echo " => failed"; \
	done

clean:
	@rm -f *.cm* *.$(O) *.$(A) $(DEPENDS) $(LINKS) lib.ml *~ *.byt* *.opt*

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
