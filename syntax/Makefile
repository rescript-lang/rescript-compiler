OCAMLOPT=ocamlopt.opt
OCAMLFLAGS= -w +a-4-42-40-9-48 -warn-error +a -bin-annot -I +compiler-libs -I src -I tests -absname
OCAMLDEP=ocamldep.opt
%.cmi : %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<
%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<
include .depend
.PHONY: depend
depend:
	$(OCAMLDEP) -native -I tests -I src src/*.ml src/*.mli tests/*.ml tests/*.mli > .depend

API_FILES = \
	src/reactjs_jsx_ppx_v3.cmx\
	src/res_io.cmx\
	src/res_minibuffer.cmx\
	src/res_doc.cmx\
	src/res_comment.cmx\
	src/res_token.cmx\
	src/res_grammar.cmx\
	src/res_reporting.cmx\
	src/res_diagnostics_printing_utils.cmx \
	src/res_diagnostics.cmx\
	src/res_parsetree_viewer.cmx\
	src/res_parens.cmx\
	src/res_comments_table.cmx\
	src/res_printer.cmx\
	src/res_scanner.cmx\
	src/res_js_ffi.cmx\
	src/res_parser.cmx\
	src/res_core.cmx\
	src/res_driver.cmx \
	src/res_ast_conversion.cmx \
	src/res_driver_ml_parser.cmx \
	src/res_driver_reason_binary.cmx \
	src/res_driver_binary.cmx \
	src/res_ast_debugger.cmx \
	src/res_outcome_printer.cmx \
	src/res_multi_printer.cmx

CLI_FILES = $(API_FILES) src/res_cli.cmx

TEST_FILES = $(API_FILES) tests/res_diff.cmx tests/res_test.cmx

.DEFAULT_GOAL := build-native

lib/rescript.exe: $(CLI_FILES)
	$(OCAMLOPT) $(OCAMLFLAGS) -O2 -o ./lib/rescript.exe -I +compiler-libs ocamlcommon.cmxa  -I src $(CLI_FILES)

build-native: lib/refmt.exe lib/rescript.exe depend

bootstrap: build-native
	ocaml unix.cma ./scripts/bspack.ml -bs-main Res_cli -I src -o ./lib/rescript.ml
	./lib/rescript.exe -parse ml -print ns ./lib/Rescript.ml > ./lib/Rescript2.ml
	$(OCAMLOPT) -w a -pp "./lib/rescript.exe -print binary" -O2 -o ./lib/rescript2.exe -I +compiler-libs ocamlcommon.cmxa -I lib ./lib/Rescript2.ml
	mv ./lib/rescript2.exe ./lib/rescript.exe

lib/refmt.exe: vendor/refmt_main3.ml
	$(OCAMLOPT) -w a -O2 -I vendor -I +compiler-libs ocamlcommon.cmxa -o lib/refmt.exe vendor/refmt_main3.ml

bench: lib/bench.exe
	./lib/bench.exe

lib/bench.exe: benchmarks/refmt_main3b.cmx benchmarks/Benchmark.ml $(API_FILES)
	$(OCAMLOPT) $(OCAMLFLAGS) -O2 -o ./lib/bench.exe -bin-annot -I +compiler-libs ocamlcommon.cmxa benchmarks/mac_osx_time.c -I benchmarks -I src $(API_FILES) benchmarks/refmt_main3b.cmx benchmarks/Benchmark.ml

benchmarks/refmt_main3b.cmx: benchmarks/refmt_main3b.ml
	$(OCAMLOPT) -c -O2 -I +compiler-libs ocamlcommon.cmxa benchmarks/refmt_main3b.ml

lib/test.exe: $(TEST_FILES)
	$(OCAMLOPT) $(OCAMLFLAGS) -O2 -o ./lib/test.exe -bin-annot -I +compiler-libs ocamlcommon.cmxa -I src -I tests $(TEST_FILES)

test: build-native lib/test.exe
	./node_modules/.bin/reanalyze -all-cmt . -suppress tests -suppress benchmarks
	./node_modules/.bin/jest
	./lib/test.exe

roundtrip-test: bootstrap lib/test.exe
	./node_modules/.bin/reanalyze -all-cmt . -suppress tests -suppress benchmarks
	ROUNDTRIP_TEST=1 ./node_modules/.bin/jest
	./lib/test.exe

reanalyze: build-native lib/test.exe
	./node_modules/.bin/reanalyze -all-cmt . -suppress tests -suppress benchmarks

clean:
	rm -rf src/*.cm*
	rm -rf src/*.o
	rm -rf tests/*.cm*
	rm -rf tests/*.o
	rm -rf benchmarks/*.cm*
	rm -rf benchmarks/*.o
	rm -rf lib/bench.exe
	rm -rf lib/rescript.exe
	rm -rf lib/test.exe
	git clean -dfx src
.PHONY: clean test roundtrip-test termination dce exception reanalyze bootstrap build-native
