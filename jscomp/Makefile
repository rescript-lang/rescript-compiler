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
json_lexer.ml: json_lexer.gen.mll
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
snapshotml:./bin/bspack.exe ./bin/compiler.mllib
	@echo "Snapshot ml"
	$< -bs-mllib bin/compiler.mllib -o bin/compiler.ml
	$< -bs-log-mllib bin/bsppx.mllib  -prelude-str 'module Config = Config_bsppx'  -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing/ -I common -I ext -I syntax -bs-main bsppx_main.ml -o bin/bsppx.ml
	$< -bs-log-mllib bin/whole_compiler.mllib -prelude-str 'module Config = Config_whole_compiler' -bs-exclude-I config -o bin/whole_compiler.ml -bs-main js_main.ml -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ../ocaml/driver/ -I ext -I syntax -I depends -I common
	BS_OCAMLDEP=true $< -bs-log-mllib bin/bsdep.mllib  -prelude-str 'module Config = Config_bsdep'  -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/driver -I common -I ext -I syntax -I depends -I .  -bs-main ../ocaml/tools/ocamldep.ml -o bin/bsdep.ml
	$< -bs-log-mllib bin/bsbuild.mllib -I common -I ext -I syntax -I depends -bs-main ./bsbuild_main.ml -o bin/bsbuild.ml
	BS_MIN_LEX_DEPS=true $< -bs-log-mllib bin/bspp.mllib   -bs-exclude-I config -I ../ocaml/utils/ -I ../ocaml/parsing?parser  -I common -I ext -I syntax -I depends -I bspp -bs-main bspp_main.ml -o bin/bspp.ml


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
	./bin/bspack.exe -bs-log-mllib bin/bspack.mllib -prelude-str 'module Config = Config_bspack '   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	$(NATIVE) -w -40  unix.cmxa  -I bin bin/config_bspack.mli bin/config_bspack.ml bin/bspack.mli bin/bspack.ml -o bin/bspack.exe
	@echo "Using the bootstrapped bspack.exe to genreate new bspack.ml: `date`"
	./bin/bspack.exe -bs-log-mllib bin/bspack.mllib -prelude-str  'module Config = Config_bspack'   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
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

## this target is 
## shared with our bundled compiler
## in this condition, `Config_whole_compiler.ml` is not configured
## we reuse the compiler-libs
world:
	@echo "Making compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc.exe
	$(MAKE) -C bin -j 4 bspack.exe bsdep.exe bsppx.exe bspp.exe bsbuild.exe
	@echo "Making compiler finished"
	$(MAKE) libs

dist-world:
	@echo "Making compiler"
	$(MAKE) -C bin -j5  all
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
	cp  ./bin/bsc.exe ./bin/bsppx.exe  ./bin/bspack.exe ./bin/bsdep.exe ./bin/bspp.exe ./bin/bsbuild.exe  $(DEST_BIN)
	@echo "copy stdlib"
	cp ./runtime/*.cmt* ./runtime/*.cmj* ./stdlib/*.cm* ./others/*.ml ./others/*.mli  ./others/*.cm* \
	 ./runtime/js.ml  ./runtime/js.cmi ./runtime/js_unsafe.cmi ./runtime/js_null.ml ./runtime/js_null.cmi \
	./runtime/js_undefined.ml ./runtime/js_undefined.cmi   $(DEST)
# ./runtime/js_null_undefined.ml ./runtime/js_null_undefined.cmi


.PHONY: test  quicktest release snapshot snapshotcmj releasebuild libs
