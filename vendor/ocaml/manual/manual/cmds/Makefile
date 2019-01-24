FILES=comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
  depend.tex profil.tex debugger.tex browser.tex ocamldoc.tex \
  warnings-help.tex ocamlbuild.tex flambda.tex spacetime.tex \
  afl-fuzz.tex plugins.tex unified-options.tex

TOPDIR=../../..
include $(TOPDIR)/Makefile.tools

TRANSF=$(OCAMLRUN) ../../tools/transf
TEXQUOTE=../../tools/texquote2
FORMAT=../../tools/format-intf

WITH_TRANSF= ocamldoc.tex top.tex intf-c.tex flambda.tex spacetime.tex \
  afl-fuzz.tex lexyacc.tex debugger.tex

etex-files: $(FILES)

all: $(FILES)

clean::
	rm -f $(FILES)
	rm -f *~ #*#

.SUFFIXES:
.SUFFIXES: .tex .etex

.etex.tex:
	@$(TEXQUOTE) < $*.etex > $*.texquote_error.tex\
	&& mv $*.texquote_error.tex $*.tex\
	|| printf "Failure when generating %s\n" $*.tex

$(WITH_TRANSF):%.tex:%.etex
	@$(TRANSF) < $*.etex > $*.transf_error.tex \
	&& mv $*.transf_error.tex $*.transf_gen.tex \
	&& $(TEXQUOTE) < $*.transf_gen.tex > $*.texquote_error.tex \
	&& mv $*.texquote_error.tex $*.tex \
	|| printf "Failure when generating %s\n" $*.tex

warnings-help.etex: ../warnings-help.etex
	cp ../warnings-help.etex .

clean::
	rm -f warnings-help.etex
