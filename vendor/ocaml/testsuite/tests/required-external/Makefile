# Ensure that calling an external C primite forces linking
# the module that defines it

MAIN_MODULE = main
LIBRARIES = lib

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.one
include $(BASEDIR)/makefiles/Makefile.common

main.cmo: lib.cma
main.cmx: lib.cmxa

lib.cma: file.cmo
	@$(OCAMLC) -a -o $@ $<

lib.cmxa: file.cmx
	@$(OCAMLOPT) -a -o $@ $<
