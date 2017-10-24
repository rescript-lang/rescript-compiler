
test: editor.opt
	./editor.opt  -rr 15 tests/divisors12.gml

editor.opt: ed_*.ml
	make -C .. editor/editor.opt

editor.byte: ed_*.ml
	make -C .. editor/editor.byte

# tortuegpoint: tortue.ml 
#	ocamlopt outils_math.ml
#	ocamlopt -o $@ graphics.cmxa outils_math.cmx $^


# graphEd.opt : outils_math.ml outils_tort.ml graphEd.ml
# 	ocamlopt -o $@ graphics.cmxa -I .. ../graph.cmxa $^


# graphEdGTK : outils_math.ml outils_tort.ml graphEdGTK.ml
# 	ocamlopt.opt -o $@ -I +lablgtk2 lablgtk.cmxa lablgnomecanvas.cmxa unix.cmxa -I .. ../graph.cmxa $^

# graphEdGTK.byte : outils_math.ml outils_tort.ml graphEdGTK.ml
# 	ocamlc.opt -g -o $@ -I +lablgtk2 lablgtk.cma lablgnomecanvas.cma unix.cma -I .. ../graph.cma $^

# editor.opt:
# 	make -C .. 

clean:
	rm -f *.opt
	rm -f *.cm[iox]
