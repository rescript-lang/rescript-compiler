ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)
BYTE=ocamlc.opt$(EXE)
OCAMLLEX=ocamllex.opt$(EXE)

CAMLP4OF=camlp4of
## native plugin was not installed in opam..

## Beging Rules only make sense in dev mode
js_map.ml:js_map.mlp j.ml
	@echo "Regenrating j_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o -o  $@

js_fold.ml:js_fold.mlp j.ml
	@echo "Regenrating j_fold.ml"
	$(CAMLP4OF) -filter Camlp4FoldGenerator  -filter trash -impl $< -printer o -o $@

sexp_lexer.ml: sexp_lexer.gen.mll
	@echo "Regenerating sexp_lexer.ml"
	$(OCAMLLEX) $< -o $@
bs_json.ml: bs_json.gen.mll
	@echo "Regenerating sexp_lexer.ml"
	$(OCAMLLEX) $< -o $@ || (rm $@ && exit 2)

# lam_map.ml: lambda_map.mlp lambda.mlp
# 	@echo "Regenrating lambda_map.ml"
# 	$(CAMLP4OF) -filter map -filter trash -impl $< -printer -o  $@

# lam_fold.ml: lambda_fold.mlp lambda.mlp
# 	@echo "Regenrating lambda_map.ml"
# 	$(CAMLP4OF) -filter map -filter trash -impl $< -printer -o  $@
## End Rules


# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent


# For faster compilation when can cache ast file instead for Linux/MacOS users
# For windows user, we just ship the binary
./bin/bsc.exe: _build/ext/ext.cmxa _build/common/common.cmxa _build/depends/depends.cmxa _build/syntax/syntax.cmxa _build/core.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa  $^  2>>build.compile

# this target  is used to make snapshotml
./bin/bspack.exe: bin/config_bspack.mli bin/config_bspack.ml bin/bspack.mli bin/bspack.ml
	$(MAKE) -C bin bspack.exe
snapshot: ./bin/bspack.exe snapshotcmj
	$(MAKE) snapshotml


# TODO
# enhace for Parser module, we only care about Parser.token 
# files including Parsetree/Parser should be shaked 
snapshotml:./bin/bspack.exe
	@echo "Snapshot ml"
	make -j6  snapshot2 snapshot3 snapshot4 snapshot5 snapshot6
snapshot2:./bin/bspack.exe 
	$< -bs-MD  -prelude-str 'module Config = Config_bsppx'  -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing/ -I common -I ext -I syntax -bs-main Bsppx_main -o bin/bsppx.ml
snapshot3:./bin/bspack.exe 
	$< -bs-MD -prelude-str 'module Config = Config_whole_compiler' -bs-exclude-I config -o bin/whole_compiler.ml -bs-main Js_main -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ../ocaml/driver/ -I ext -I syntax -I depends -I common
snapshot4:./bin/bspack.exe 
	BS_OCAMLDEP=true $< -bs-MD  -prelude-str 'module Config = Config_bsdep'  -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/driver -I ../ocaml/tools -I common -I ext -I syntax -I depends  -bs-main Ocamldep -o bin/bsdep.ml
snapshot5:./bin/bspack.exe 
	$< -bs-MD -I common -I ext -I syntax -I depends -bs-main Bsb_main -o bin/bsb.ml
snapshot6:./bin/bspack.exe 
	BS_MIN_LEX_DEPS=true $< -bs-MD   -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing?parser  -I common -I ext -I syntax -I depends -I bspp -bs-main Bspp_main -o bin/bspp.ml


snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 -lflags -I,+compiler-libs,ocamlcommon.cmxa js_pack.native --

# git diff bin/bspack.ml
# bootbspack requires ../ocaml has parser, lexer processed by ocamlyacc and ocamllex


# bootbspack should not depend on bspack.exe
# if so, `make bootbspack` will cause `bspack.exe` to be rebuilt
# which may already be wrong
bootbspack: 
	@echo "Bootstrap bspack.exe"
	./bin/bspack.exe -bs-MD -prelude-str 'module Config = Config_bspack '   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main Bspack_main -o bin/bspack.ml
	$(NATIVE) -w -40  unix.cmxa  -I bin bin/config_bspack.mli bin/config_bspack.ml bin/bspack.mli bin/bspack.ml -o bin/bspack.exe
	@echo "Using the bootstrapped bspack.exe to genreate new bspack.ml: `date`"
	./bin/bspack.exe -bs-MD  -prelude-str  'module Config = Config_bspack'   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main Bspack_main -o bin/bspack.ml
	@echo "Bootstrap seems finished, please check diffs in bspack.exe"
	@echo "Rebuilding bspack.exe"
	make -C bin -B bspack.exe
	@echo "Rebuilding bspack.exe finsihed"

libs:
	@echo "Making stdlib cmis"
	cd stdlib && $(MAKE) allcmis
	@echo "Making stdlib finished"

	@echo "Making runtime"
	cd runtime && $(MAKE) all
	@echo "Making runtime finished"


	@echo "Making stdlib"
	cd stdlib && $(MAKE) all
	@echo "Making stdlib finished"

	@echo "Making others"
	cd others && $(MAKE) all
	@echo "Making others finished"

releasebuild:
	@echo "Make release compiler"
	$(MAKE) -C bin all 

release:snapshot
	$(MAKE) releasebuild


world:
	@echo "Making compiler"
	$(MAKE) -C bin -j 6 all
	@echo "Making compiler finished"
	$(MAKE) libs


travis-world-test:
	$(MAKE) -C bin bspack.exe
	@echo "Generating the compiler"
	rm -f bin/compiler.ml bin/bsppx.ml
	$(MAKE) snapshotml
	@echo "Generating the compiler finished"
	$(MAKE) world
	@echo "Making test"
	cd test && $(MAKE) all
	@echo "Making test finsihed"


DEST=../lib/ocaml
DEST_BIN=../bin
install:
	@echo "copy exe"
	mkdir -p $(DEST_BIN) $(DEST)
	cp  ./bin/bsc.exe ./bin/bsppx.exe  ./bin/bspack.exe ./bin/bsdep.exe ./bin/bspp.exe ./bin/bsb  $(DEST_BIN)
	@echo "copy stdlib"
	cp ./runtime/*.cmt* ./runtime/*.cmj* ./stdlib/*.cm* ./others/*.ml ./others/*.mli  ./others/*.cm* \
	 ./runtime/js.ml  ./runtime/js.cmi ./runtime/js_unsafe.cmi ./runtime/js_null.ml ./runtime/js_null.cmi \
	./runtime/js_undefined.ml ./runtime/js_undefined.cmi   $(DEST)
# ./runtime/js_null_undefined.ml ./runtime/js_null_undefined.cmi


.PHONY: test  quicktest release snapshot snapshotcmj releasebuild libs snapshot1 snapshot2 snapshot3 snapshot4 snapshot5 snapshot6
