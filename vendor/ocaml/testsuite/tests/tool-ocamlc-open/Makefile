BASEDIR=../..

compile:
	@printf " ... testing 'foo.ml'"
	@$(OCAMLC) -c a.ml
	@$(OCAMLC) -open A.M -c b.ml \
	&& echo " => passed" || echo " => failed"

promote:

clean:
	@rm -f a.cmi a.cmo b.cmi b.cmo

include $(BASEDIR)/makefiles/Makefile.common
