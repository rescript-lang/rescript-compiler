
EPS= images/lentille1.eps images/lentille2.eps images/perpendiculaire.eps images/parallels.eps  

all : rapport.ps soutenance.pdf

soutenance.pdf: soutenance.tex
	pdflatex soutenance.tex
	xpdf soutenance.pdf

rapport.ps: rapport.tex biblio.bib $(EPS)
	latex rapport
	bibtex rapport
	latex rapport
	latex rapport
	dvips rapport.dvi -o $@

rapport-pp.tex: pp.ml rapport.tex	 
	ocaml $^ > $@

pp.ml: pp.mll
	ocamllex pp.mll

images: $(EPS)

%.eps: %.png
	sam2p $^ EPS: $@

print: rapport.ps
	lpr $^
clean:
	rm -f *.ml *.tex  *.log *.dvi