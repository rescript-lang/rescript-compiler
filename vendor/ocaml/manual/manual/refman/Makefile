FILES= refman.tex lex.tex names.tex values.tex const.tex types.tex \
  patterns.tex expr.tex typedecl.tex modtypes.tex modules.tex compunit.tex \
  exten.tex classes.tex

TOPDIR=../../..

include $(TOPDIR)/Makefile.tools

CAMLLATEX= $(OCAMLRUN) ../../tools/caml-tex2 -caml "TERM=norepeat $(OCAML)" \
-n 80 -v false
TRANSF=../../tools/transf
TEXQUOTE=../../tools/texquote2

ALLFILES=$(FILES)

etex-files: $(ALLFILES)
all: $(ALLFILES)

clean:
	rm -f $(ALLFILES)

.SUFFIXES:
.SUFFIXES: .etex .tex

exten.tex:exten.etex
	@$(CAMLLATEX) -o $*.caml_tex_error.tex $*.etex \
	&& mv $*.caml_tex_error.tex $*.gen.tex \
	&& $(OCAMLRUN) $(TRANSF) < $*.gen.tex > $*.transf_error.tex \
	&& mv $*.transf_error.tex $*.gen.tex\
	&& $(TEXQUOTE) < $*.gen.tex  > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex
.etex.tex:
	@$(OCAMLRUN) $(TRANSF) < $*.etex > $*.transf_error.tex \
	&& mv $*.transf_error.tex $*.gen.tex\
	&& $(TEXQUOTE) < $*.gen.tex  > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex


$(ALLFILES): $(TRANSF) $(TEXQUOTE)
