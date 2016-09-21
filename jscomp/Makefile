ifdef WIN32
	EXE := .exe
endif
ifndef EXE
	EXE := # empty
endif
NATIVE=ocamlopt.opt$(EXE)
BYTE=ocamlc.opt$(EXE)
CAMLP4OF=camlp4of.opt$(EXE)
## Beging Rules only make sense in dev mode
js_map.ml:js_map.mlp j.ml
	@echo "Regenrating j_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@

js_fold.ml:js_fold.mlp j.ml
	@echo "Regenrating j_fold.ml"
	$(CAMLP4OF) -filter fold  -filter trash -impl $< -printer o > $@

lam_map.ml: lambda_map.mlp lambda.mlp
	@echo "Regenrating lambda_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@

lam_fold.ml: lambda_fold.mlp lambda.mlp
	@echo "Regenrating lambda_map.ml"
	$(CAMLP4OF) -filter map -filter trash -impl $< -printer o > $@
## End Rules


# TODO: should be done after we do the build
# Carefully check the stored data if it is platform independent

./bin/bspack.exe: ./bin/bspack_config.mli ./bin/bspack_config.ml ./bin/bspack.mli ./bin/bspack.ml
	$(NATIVE) -w -a -I bin  unix.cmxa  $^ -o $@

# For faster compilation when can cache ast file instead for Linux/MacOS users
# For windows user, we just ship the binary
./bin/bsc.exe: _build/ext/ext.cmxa _build/common/common.cmxa _build/depends/depends.cmxa _build/syntax/syntax.cmxa _build/core.cmxa
	echo "Linking"
	$(NATIVE) -g -linkall -o $@ -I +compiler-libs ocamlcommon.cmxa  $^  2>>build.compile

./bin/bsppx.exe: ./bin/bsppx.mli ./bin/bsppx.ml
	$(NATIVE) -g -inline 1000 -linkall -w -a -I bin -I +compiler-libs ocamlcommon.cmxa   $^ -o $@


snapshot: ./bin/bspack.exe snapshotcmj
	$(MAKE) snapshotml

snapshotml:./bin/bspack.exe ./bin/compiler.mllib
	@echo "Snapshot ml"
	$< -bs-mllib bin/compiler.mllib -o bin/compiler.ml
	$< -I common -I ext -I syntax -bs-main bsppx_main.ml -o bin/bsppx.ml
	$< -bs-log-mllib bin/whole_compiler.mllib -prelude-str 'module Config = Whole_compiler_config ' -bs-exclude-I config -o bin/whole_compiler.ml -bs-main js_main.ml -I ../ocaml/utils/ -I ../ocaml/parsing/ -I ../ocaml/typing/ -I ../ocaml/bytecomp/ -I ../ocaml/driver/ -I ext -I syntax -I depends -I common

snapshotcmj:
	@echo "Collecting cmj files"
	ocamlbuild -cflags $(OCAMLBUILD_CFLAGS)	 -lflags -I,+compiler-libs,ocamlcommon.cmxa js_pack.native --

# git diff bin/bspack.ml
# bootbspack requires ../ocaml has parser, lexer processed by ocamlyacc and ocamllex
bootbspack: ./bin/bspack.exe
	@echo "Bootstrap bspack.exe"
	./bin/bspack.exe -bs-log-mllib bin/bspack.mllib -prelude-str 'module Config = Bspack_config '   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	$(NATIVE) -w -40  unix.cmxa  -I bin bin/bspack_config.mli bin/bspack_config.ml bin/bspack.mli bin/bspack.ml -o bin/bspack.exe
	@echo "Using the bootstrapped bspack.exe to genreate new bspack.ml: `date`"
	./bin/bspack.exe -bs-log-mllib bin/bspack.mllib -prelude-str  'module Config = Bspack_config '   -bs-exclude-I config  -I ../ocaml/parsing/ -I ../ocaml/utils/ -I ext -I common -I depends -bs-main bspack_main.ml -o bin/bspack.ml
	@echo "Bootstrap seems finished, please check diffs in bspack.exe"
	@echo "Rebuilding bspack.exe"
	make -B ./bin/bspack.exe
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

releasebuild:./bin/bspack.exe
	@echo "Make release compiler"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/compiler.mli bin/compiler.ml -o bin/bsc.exe
	@echo "Make release ppx"
	$(NATIVE) -g -inline 1000 -linkall  -w -a -I +compiler-libs -I bin ocamlcommon.cmxa  bin/bsppx.mli bin/bsppx.ml -o bin/bsppx.exe

release:snapshot
	$(MAKE) releasebuild

world:
	@echo "Making compiler"
	$(MAKE) releasebuild
	@echo "Making compiler finished"
	$(MAKE) libs

dist-world:
	@echo "Making compiler"
	$(NATIVE) -w -a -I bin  ./bin/whole_compiler_config.mli ./bin/whole_compiler_config.ml  ./bin/whole_compiler.mli ./bin/whole_compiler.ml -o bin/bsc.exe
	$(MAKE) bin/bspack.exe
	$(MAKE) bin/bsppx.exe
	$(MAKE) libs


travis-world-test:./bin/bspack.exe
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
	cp  ./bin/bsc.exe ./bin/bsppx.exe  ./bin/bspack.exe $(DEST_BIN)
	@echo "copy stdlib"
	cp ./runtime/*.cmt* ./runtime/*.cmj* ./stdlib/*.cm* ./others/*.ml ./others/*.mli  ./others/*.cm* \
	 ./runtime/js.ml  ./runtime/js.cmi ./runtime/js_unsafe.cmi ./runtime/js_null.ml ./runtime/js_null.cmi \
	./runtime/js_undefined.ml ./runtime/js_undefined.cmi ./runtime/js_null_undefined.ml ./runtime/js_null_undefined.cmi  $(DEST)


.PHONY: test  quicktest release snapshot snapshotcmj releasebuild libs
