# Tests for compilation with missing cmis
# main.ml: error message when equality is missing
# main_ok.ml: allow path expansion even when the target is missing (GPR#816)

SOURCES = subdir/m.ml a.ml b.ml c.ml main.ml main_ok.ml

.PHONY: default
default: $(SOURCES)
	@printf " ... testing 'main.ml'";
	@$(OCAMLC) -c subdir/m.ml;
	@$(OCAMLC) -c -I subdir a.ml;
	@$(OCAMLC) -c -I subdir b.ml;
	@$(OCAMLC) -c -I subdir c.ml;
	@$(OCAMLC) -c main.ml > main.ml.result 2>&1 || :
	@$(DIFF) main.ml.result main.ml.reference >/dev/null \
	&& echo " => passed" || echo " => failed"
	@printf " ... testing 'main_ok.ml'";
	@$(OCAMLC) -c main_ok.ml && echo " => passed" || echo " => failed"

.PHONY: clean
clean:
	@rm -f subdir/m.cm[io] *.cm[io] main.ml.result

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
