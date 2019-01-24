# Check safety of linking

SOURCES = a.ml b_bad.ml
OBJECTS = $(SOURCES:%.ml=%.cmo)

all: a.cmo
	@printf " ... testing 'b_bad.ml'"
	@$(OCAMLC) $(ADD_COMPFLAGS) -c -safe-string -warn-error +8 b_bad.ml 2> /dev/null \
	 && echo " => failed" || echo " => passed"

clean:
	@rm -f *.cmo *.cmi

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
