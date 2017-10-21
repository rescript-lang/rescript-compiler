
all: main.ps

main.ps: main.tex
	latex main
	bibtex main
	latex main
	latex main
	dvips main.dvi -o $@

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

export: main.ps
	cp main.ps $$HOME/ftp/publis/ocamlgraph.ps
