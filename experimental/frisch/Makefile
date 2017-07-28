ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/typing -I $(ROOT)/driver -I $(ROOT)/toplevel -w A-4-9-42
COMMON=$(ROOT)/compilerlibs/ocamlcommon.cma
BYTECMP=$(ROOT)/compilerlibs/ocamlbytecomp.cma
TOPLVL=$(ROOT)/compilerlibs/ocamltoplevel.cma

clean:
	rm -f *.exe *.cm* *~

## Detecting unused exported values

.PHONY: unused_exported_values
unused_exported_values:
	$(OCAMLC) -o unused_exported_values.exe $(COMMON) $(ROOT)/tools/tast_iter.cmo unused_exported_values.ml


## Conditional compilation based on environment variables

.PHONY: ifdef
ifdef:
	$(OCAMLC) -o ifdef.exe $(COMMON) ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe -dsource test_ifdef.ml
	./test_ifdef.exe

## A proposal for replacing js_of_ocaml Camlp4 syntax extension with
## a -ppx filter

.PHONY: js_syntax
js_syntax:
	$(OCAMLC) -o js_syntax.exe $(COMMON) js_syntax.ml
	$(OCAMLC) -o test_ifdef.exe -i -ppx ./js_syntax.exe test_js.ml


## A "toy" ocamldoc clone based on .cmti files

.PHONY: minidoc
minidoc:
	$(OCAMLC) -custom -o minidoc.exe $(COMMON) minidoc.ml
	$(OCAMLC) -c -bin-annot testdoc.mli
	./minidoc.exe testdoc.cmti

## Using the OCaml toplevel to evaluate expression during compilation

.PHONY: eval
eval:
	$(OCAMLC) -linkall -o eval.exe  $(COMMON) $(BYTECMP) $(TOPLVL) eval.ml
	$(OCAMLC) -o test_eval.exe -ppx ./eval.exe test_eval.ml
	./test_eval.exe

## Example of code generation based on type declarations

.PHONY: ppx_builder
ppx_builder:
	$(OCAMLC) -linkall -o ppx_builder.exe  $(COMMON) ppx_builder.ml
	$(OCAMLC) -o test_builder.exe -ppx ./ppx_builder.exe -dsource test_builder.ml

## Import type definitions from other source files (e.g. to avoid code
## duplication between the .ml and .mli files)

.PHONY: copy_typedef
copy_typedef:
	$(OCAMLC) -linkall -o copy_typedef.exe  $(COMMON) copy_typedef.ml
	$(OCAMLC) -c -ppx ./copy_typedef.exe test_copy_typedef.mli
	$(OCAMLC) -o test_copy_typedef.exe -ppx ./copy_typedef.exe -dsource test_copy_typedef.ml


## Create mli files from ml files

.PHONY: nomli
nomli:
	$(OCAMLC) -linkall -o nomli.exe $(COMMON) $(BYTECMP) ../../tools/untypeast.cmo ../../tools/tast_iter.cmo nomli.ml
	./nomli.exe test_nomli.ml

## A port of pa_matches

.PHONY: matches
matches:
	$(OCAMLC) -linkall -o ppx_matches.exe $(COMMON) ppx_matches.ml
	$(OCAMLC) -c -dsource -ppx ./ppx_matches.exe test_matches.ml
