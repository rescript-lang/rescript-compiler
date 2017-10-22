
all: cameraready.pdf

cameraready.pdf: cameraready.tex paper.tex macros.tex interface.pdf paper.bib
	pdflatex cameraready
	bibtex cameraready
	pdflatex cameraready
	pdflatex cameraready

old: main-pp.ps cameraready.ps

cameraready.ps: cameraready.tex paper.tex paper.bib
	latex cameraready
	bibtex cameraready
	latex cameraready
	latex cameraready
	dvips cameraready -o $@


main-pp.ps: main-pp.tex
	latex main-pp
	bibtex main-pp
	latex main-pp
	latex main-pp
	dvips main-pp.dvi -o $@

OCAMLOPT=ocamlopt.opt -unsafe -inline 1000

lab.opt: lab.ml
	$(OCAMLOPT) -I .. unix.cmxa graphics.cmxa ../graph.cmxa -o $@ $^

maze.opt: maze.ml
	$(OCAMLOPT) -I .. unix.cmxa graphics.cmxa ../graph.cmxa -o $@ size.ml $^

bench_maze: maze.opt
	./maze.opt

bench_build: bench_build.opt
	./bench_build.opt

bench_build.opt: bench_build.ml
	$(OCAMLOPT) -I .. unix.cmxa ../graph.cmxa -o $@ size.ml $^

bench_build.byte: bench_build.ml
	ocamlc -g -I .. unix.cma ../graph.cma $^
	ocamlrun -bt ./a.out

export: main-pp.ps
	cp main-pp.ps $$HOME/ftp/publis/ocamlgraph-tfp07.ps

%-pp.tex: pp.ml %.tex 
	ocaml $^ > $@

pp.ml: pp.mll
	ocamllex pp.mll
