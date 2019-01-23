all: belt_ast_exp_extension.ml belt_ast_tdcls.ml belt_ast_attributes.ml           belt_ast_util.ml belt_ast_core_type_class_type.ml belt_bsppx_main.ml belt_ast_derive_abstract.ml belt_ppx_entry.ml belt_ast_derive_projector.ml belt_ast_exp_apply.ml
	../../bin/bspack.exe -U BS_DEBUG -bs-MD  -module-alias Config=Config_whole_compiler  -I ../../../vendor/ocaml/utils -I ../../../vendor/ocaml/parsing  -I ../../stubs -I ../../common -I ../../ext -I ../../syntax -I ../../core -bs-main Belt_bsppx_main -o ../../../lib/belt_bsppx.ml
	make -C ../../../lib belt_bsppx.exe

test:
	ocamlc.opt -ppx ../../../lib/belt_bsppx.exe -I ../ -c test.mli
	ocamlc.opt -ppx ../../../lib/belt_bsppx.exe -I ../ -c test.ml
	ocamlc.opt -ppx ../../../lib/belt_bsppx.exe -I ../ -c test_use.ml
	ocamlc.opt ../js.cmo test.cmo test_use.cmo
	./a.out
