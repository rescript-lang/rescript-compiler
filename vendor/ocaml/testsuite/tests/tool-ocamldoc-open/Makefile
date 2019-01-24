BASEDIR=../..
COMPFLAGS=-I $(OTOPDIR)/ocamldoc
LD_PATH=$(TOPDIR)/otherlibs/$(UNIXLIBVAR)unix:$(TOPDIR)/otherlibs/str
DOCFLAGS=-I $(OTOPDIR)/stdlib $(COMPFLAGS)

SRC= main.ml alias.ml inner.ml
ODOCS=$(SRC:%.ml=%.odoc)

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo 'skipped (shared libraries not available)'; \
	else \
	  $(SET_LD_PATH) $(MAKE) doc; \
	fi

.PHONY: doc
doc: $(ODOCS)
	@printf " ... testing ocamldoc '-open' option";\
	$(OCAMLDOC) $(DOCFLAGS) -hide-warnings \
         -load alias.odoc -load inner.odoc \
         -load main.odoc -latex -o doc.result ;\
	$(DIFF) doc.result doc.reference > /dev/null \
	&& echo " => passed" || echo " => failed";

inner.odoc: inner.ml
	@$(OCAMLDOC) $(DOCFLAGS) -hide-warnings \
        -dump inner.odoc inner.ml

alias.odoc: inner.cmi alias.ml
	@$(OCAMLDOC) $(DOCFLAGS) -hide-warnings \
        -dump alias.odoc alias.ml

main.odoc: alias.cmi main.ml
	@$(OCAMLDOC) $(DOCFLAGS) -hide-warnings \
        -open Alias.Container -open Aliased_inner -dump main.odoc main.ml

alias.cmi:inner.cmi

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.odoc *.toc *.sty *.aux *.log *.result

include $(BASEDIR)/makefiles/Makefile.common
