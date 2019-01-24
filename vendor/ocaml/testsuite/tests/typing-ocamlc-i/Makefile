# Check ocamlc -i

SOURCES = pr7620_bad.ml

all: 
	@printf " ... testing '$(SOURCES)'"
	@$(OCAMLC) -i $(SOURCES) 2> /dev/null \
	 && echo " => failed" || echo " => passed"

clean: defaultclean
	@rm -f *~

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common

