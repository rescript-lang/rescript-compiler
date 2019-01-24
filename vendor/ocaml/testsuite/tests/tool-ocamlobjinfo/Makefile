BASEDIR=../..

LD_PATH=

# This test ensures that ocamlobjinfo is behaving as the configuration
# expects and is a guard against the breakage fixed in 17fc532

.PHONY: default
default:
	@printf " ... testing 'ocamlobjinfo'"
	@if ! $(SUPPORTS_SHARED_LIBRARIES) || $(BYTECODE_ONLY) ; then \
	  echo ' => skipped (.cmxs not built)'; \
	elif ! grep -q HAS_LIBBFD $(TOPDIR)/byterun/caml/s.h ; then \
	  echo ' => skipped (BFD library not available)'; \
	else \
    $(SET_LD_PATH) OCAMLLIB=$(TOPDIR)/tools $(MAKE) run; \
	fi

.PHONY: run
run:
	@rm -f $(MAIN_MODULE).result
	@$(OCAMLOPT) -shared -o question.cmxs question.ml
	@$(OCAMLRUN) `$(CYGPATH) $(TOPDIR)/tools/ocamlobjinfo` \
	             question.cmxs \
	             > test.raw.result 2>&1 \
	 && sed -e 's/\([^0-9a-z]\)[0-9a-z]\{32\}\([^0-9a-z]\|$$\)/\1<MD5>\2/' \
	             test.raw.result > test.result \
	 && $(DIFF) test.reference test.result > /dev/null \
	 && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common
