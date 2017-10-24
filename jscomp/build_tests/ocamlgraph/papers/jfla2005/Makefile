ARTICLE=conchon
DVIPS=dvips
OSTYLES=$(HOME)/lib/tex/STYLES
LATEX=TEXINPUTS=$(OSTYLES):/usr/share/texmf//:$$TEXINPUTS; \
export TEXINPUTS; latex
BIBTEX=bibtex

main.ps: main.tex
	latex main
	bibtex main
	latex main
	latex main
	dvips -o $@ main.dvi

main.dvi: main.tex
	latex main

submit:
	mkdir $(ARTICLE)
	cp -f *.tex *.bbl $(ARTICLE)
	tar cf $(ARTICLE).tar $(ARTICLE)
	gzip --best $(ARTICLE).tar
	uuencode $(ARTICLE).tar.gz $(ARTICLE).tar.gz > $(ARTICLE).tar.gz.uu

export: main.ps
	gzip --best -c main.ps > $(HOME)/ftp/publis/jfla05.ps.gz
