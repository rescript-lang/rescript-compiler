# $Id$

FILES=allfiles.tex biblio.tex foreword.tex version.tex warnings-help.etex
TEXINPUTS=.:..:../refman:../library:../cmds:../tutorials:../../styles:
TEXFONTS=../../styles:
RELEASE=$$HOME/release/$${RELEASENAME}
HEVEA=hevea
HACHA=hacha
INFO=-fix -exec xxdate.exe -info -w 79
HTML=-fix -exec xxdate.exe -O
TEXT=-fix -exec xxdate.exe -text -w 79
SRC = $(abspath ../../)

export LD_LIBRARY_PATH ?= $(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/
export DYLD_LIBRARY_PATH ?= $(SRC)/otherlibs/unix/:$(SRC)/otherlibs/str/

OCAMLDOC=$(if $(wildcard $(SRC)/ocamldoc/ocamldoc.opt),\
 $(SRC)/ocamldoc/ocamldoc.opt,\
 $(SRC)/byterun/ocamlrun $(SRC)/ocamldoc/ocamldoc)\
 -hide Pervasives

MLIS=$(SRC)/stdlib/*.mli \
	$(SRC)/utils/*.mli \
	$(SRC)/parsing/*.mli \
	$(SRC)/driver/pparse.mli \
	$(SRC)/typing/typemod.mli \
	$(SRC)/bytecomp/simplif.mli \
	$(SRC)/otherlibs/bigarray/bigarray.mli \
	$(SRC)/otherlibs/dynlink/dynlink.mli \
	$(SRC)/otherlibs/graph/graphics.mli \
	$(SRC)/otherlibs/graph/graphicsX11.mli \
	$(SRC)/otherlibs/str/*.mli \
	$(SRC)/otherlibs/systhreads/*.mli \
	$(SRC)/otherlibs/unix/*.mli

manual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) latex manual.tex

index::
	cd texstuff && \
	  sh ../../tools/fix_index.sh manual.idx && \
	  makeindex manual.idx
	cd texstuff; makeindex manual.kwd.idx

pdfmanual: files
	cd texstuff; \
	TEXINPUTS=$(TEXINPUTS) pdflatex pdfmanual.tex

index::
	cd texstuff && \
	  sh ../../tools/fix_index.sh pdfmanual.idx && \
	  makeindex pdfmanual.idx
	cd texstuff; makeindex pdfmanual.kwd.idx

html: files
	cd htmlman; \
	mkdir -p libref ; \
	$(OCAMLDOC) -colorize-code -sort -html \
	-d libref \
	-I $(SRC)/stdlib  \
	-I $(SRC)/utils  \
	-I $(SRC)/parsing \
	-I $(SRC)/driver \
	-I $(SRC)/typing \
	-I $(SRC)/bytecomp \
	-I $(SRC)/otherlibs/bigarray \
	-I $(SRC)/otherlibs/dynlink \
	-I $(SRC)/otherlibs/graph \
	-I $(SRC)/otherlibs/str \
	-I $(SRC)/otherlibs/systhreads \
	-I $(SRC)/otherlibs/unix \
	$(MLIS) ; \
	cp -f ../style.css libref ; \
	${HEVEA} ${HTML} -I .. -I ../refman -I ../library -I ../cmds \
	         -I ../tutorials -I ../../styles -I ../texstuff manual.hva \
	         -e macros.tex ../manual.tex ; \
	${HACHA} -tocter manual.html ; \

info: files
	cd infoman; rm -f ocaml.info*; \
	${HEVEA} ${INFO} -o ocaml.info.body -I .. -I ../refman -I ../library \
	         -I ../cmds -I ../tutorials -I ../../styles -I ../texstuff \
	         ../manual.inf -e macros.tex ../manual.tex
	cat manual.info.header infoman/ocaml.info.body > infoman/ocaml.info
	cd infoman; rm -f ocaml.info.tmp ocaml.info.body ; gzip -9 ocaml.info*

text: files
	cd textman; \
	${HEVEA} ${TEXT} -I .. -I ../refman -I ../library -I ../cmds \
	         -I ../tutorials -I ../../styles -I ../texstuff \
	         ../manual.inf -e macros.tex ../manual.tex

etex-files: $(FILES)
	cd refman; $(MAKE) etex-files RELEASEDIR=$(SRC)
	cd library; $(MAKE) etex-files RELEASEDIR=$(SRC)
	cd cmds; $(MAKE) etex-files RELEASEDIR=$(SRC)
	cd tutorials; $(MAKE) etex-files RELEASEDIR=$(SRC)

files: $(FILES)
	cd refman; $(MAKE) all RELEASEDIR=$(SRC)
	cd library; $(MAKE) all RELEASEDIR=$(SRC)
	cd cmds; $(MAKE) all RELEASEDIR=$(SRC)
	cd tutorials; $(MAKE) all RELEASEDIR=$(SRC)

all:
	$(MAKE) manual pdfmanual RELEASEDIR=$(SRC)
	$(MAKE) manual pdfmanual RELEASEDIR=$(SRC)
	$(MAKE) index RELEASEDIR=$(SRC)
	$(MAKE) manual pdfmanual RELEASEDIR=$(SRC)
	$(MAKE) html text info RELEASEDIR=$(SRC)

clean:
	rm -f $(FILES)
	cd refman; $(MAKE) clean
	cd library; $(MAKE) clean
	cd cmds; $(MAKE) clean
	cd tutorials; $(MAKE) clean
	-rm -f texstuff/*
	cd htmlman; rm -rf libref index.html manual*.html *.haux *.hind
	cd textman; rm -f manual.txt *.haux *.hind
	cd infoman; rm -f ocaml.info ocaml.info-*  *.haux *.hind
	rm -f warnings-help.etex

release:
	gzip < texstuff/manual.dvi > $(RELEASE)refman.dvi.gz
	dvips -o '!gzip > $(RELEASE)refman.ps.gz' texstuff/manual.dvi
	cp htmlman/manual.html $(RELEASE)refman.html
	rm -f htmlman/manual.{html,haux,hmanual*,htoc}
	tar zcf $(RELEASE)refman-html.tar.gz htmlman/*.* htmlman/libref
	zip -8 $(RELEASE)refman-html.zip htmlman/*.* htmlman/libref/*.*
	cp texstuff/pdfmanual.pdf $(RELEASE)refman.pdf
	cp textman/manual.txt $(RELEASE)refman.txt
	tar cf - infoman/ocaml.info* | gzip > $(RELEASE)refman.info.tar.gz

.SUFFIXES:
.SUFFIXES: .tex .etex .htex

.etex.tex:
	../tools/texquote2 < $*.etex > $*.tex

version.tex: $(SRC)/VERSION
	sed -n -e '1s/^\([0-9]*\.[0-9]*\).*$$/\\def\\ocamlversion{\1}/p' \
	    $(SRC)/VERSION > version.tex

warnings-help.etex: $(SRC)/utils/warnings.ml $(SRC)/ocamlc
	(echo "% This file is generated from (ocamlc -warn-help)";\
	 echo "% according to a rule in manual/manual/Makefile.";\
	 echo "% In particular, the reference to documentation sections";\
	 echo "% are inserted through the Makefile, which should be updated";\
	 echo "% when a new warning is documented.";\
	 echo "%";\
	$(SRC)/boot/ocamlrun $(SRC)/ocamlc -warn-help \
	| sed -e 's/^ *\([0-9A-Z][0-9]*\)\(.*\)/\\item[\1] \2/'\
	) >$@
#	sed --inplace is not portable, emulate
	for i in 52 57; do\
		sed\
		  s'/\\item\['$$i'\]/\\item\['$$i' (see \\ref{ss:warn'$$i'})\]/'\
                  $@ > $@.tmp;\
		mv $@.tmp $@;\
	done
