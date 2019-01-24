BASEDIR=../..

.PHONY: default
default:
	@printf " ... testing -compat-32"
	@if ($(OCAMLC) -config | grep "word_size: *64") \
	then $(MAKE) run; \
	else echo ' => skipped (not compiled in 64bit)'; \
	fi

.PHONY: run
run:
	@$(OCAMLC) -compat-32 -c a.ml > test.result 2>&1 || true
	@$(OCAMLC) -c a.ml
	@$(OCAMLC) -compat-32 -a a.cmo -o a.cma >> test.result 2>&1 || true
	@$(OCAMLC) -a a.cmo -o a.cma
	@$(OCAMLC) -compat-32    a.cma -o a.byte -linkall >> test.result 2>&1 || true
	@$(DIFF) test.reference test.result >/dev/null \
	&& echo " => passed" || echo " => failed"; \

promote: defaultpromote

clean: defaultclean

include $(BASEDIR)/makefiles/Makefile.common
