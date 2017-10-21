all: slides.pdf

slides.pdf: slides.tex
	pdflatex $<

print: slides.pdf
	pdfnup --nup 2x4 slides.pdf 
	xpdf slides-2x4.pdf

